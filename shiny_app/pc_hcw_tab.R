## Primary Care and HCW Tab


### Functions ----
build_pc_hcw_tables <- function(data, dom_elements = "tB",nrow=NULL, ...){
  datatable(data, 
            style = "bootstrap",
            extensions = 'Buttons',
            options = list(dom = dom_elements,
                           pageLength = nrow,
                           buttons = list(
                             list(
                               extend = "copy", 
                               className = "btn btn-primary",
                               exportOptions = list(
                                 modifier = list(
                                   page="all", search ="none"
                                 ))
                             ),
                             list(
                               extend = "csv",
                               className = "btn btn-primary",
                               exportOptions = list(
                                 modifier = list(
                                   page="all", search ="none"
                                 ))
                             ),
                             list(
                               extend = "excel",
                               className = "btn btn-primary",
                               exportOptions = list(
                                 modifier = list(
                                   page="all", search ="none"
                                 ))
                             )),
                           columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(data)-1)),
                                             list(className = 'dt-left', targets = 0)),
                           initComplete = JS(paste0("function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#433684', 'color': '#ffffff'});}"))),
            rownames = FALSE,
            class = "table-bordered table-hover", ...)
}

completeness_bar <- function(c){
  
  data = tibble(x = c(1,1),
                y = c(c, 100-c),
                lab = factor(c("Complete", "Incomplete"),
                             levels  = c("Complete", "Incomplete")))
  
  axis_labs <- paste0(format(round_half_up(c,2), nsmall = 2),"%")
  
  ggplot(data, aes(x = x, y = y, 
                   fill = factor(lab, levels = c("Incomplete", "Complete")))) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = c("#ababab", "#433684")) +
    scale_y_continuous(breaks = c/100, labels = axis_labs) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0,vjust = 0.5, size = 14),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank()) +
    labs(x = "Completeness") +
  
    coord_flip()

}

### Key Points/ Non-reactive Objects ----

## Primary Care
pc_completeness <- 100*(1-pc_test_reasons[1,2]/sum(pc_test_reasons[,2])) %>%
  as.numeric() 
pc_completeness_lab <- paste("Please Note: The Job Role field is", 
                          paste0(format(round_half_up(pc_completeness,2), nsmall = 2), "%"),
                          "complete for Primary Care Worker Tests (all Scotland):")
pc_test_reasons_format <- pc_test_reasons %>%
  mutate(number_of_tests = trimws(format(number_of_tests, big.mark=","))) %>%
  rename(`Job Role` = test_cohort_sub_category,
         `Total Tests` = number_of_tests)


hcw_completeness <- hcw_test_reasons %>%
  ungroup() %>%
  mutate(number_of_tests = as.numeric(number_of_tests),
         flag = if_else(test_cohort_sub_category == "Not Recorded", 
                        number_of_tests,0)) %>%
  summarise(total = sum(number_of_tests), not_recorded = sum(flag)) %>%
  mutate(perc = 100*(1-not_recorded/total)) %>%
  pull(perc)

hcw_completeness_lab <- paste("Please Note: The Job Role field is", 
                             paste0(format(round_half_up(hcw_completeness,2), nsmall = 2), "%"),
                             "complete for Healthcare Worker Tests (all Scotland):")

hcw_test_reasons_format <- hcw_test_reasons %>% ungroup() %>%
  mutate(number_of_tests = trimws(format(number_of_tests, big.mark=","))) %>%
  rename(`Job Role` = test_cohort_sub_category,
         `Total Tests` = number_of_tests,
         `Health Board` = subject_residence_nhs_board) %>%
  select(2,1,3)

hcw_test_reasons_hosp_format <- hcw_test_reasons_hosp %>% ungroup() %>%
  mutate(number_of_tests = trimws(format(number_of_tests, big.mark=","))) %>%
  rename(`Job Role` = test_cohort_sub_category,
         `Total Tests` = number_of_tests,
         `Health Board` = subject_residence_nhs_board,
         Location = organisation_name) %>%
  select(2,1,3,4)

hcw_job_roles <- hcw_test_reasons$test_cohort_sub_category %>% unique
hcw_hb <- hcw_test_reasons$subject_residence_nhs_board %>% unique

### Build UI ----
output$pc_hwc_tab_widget_ui <- renderUI({ 
  if(input$pc_hcw_radio_select == "Healthcare Workers"){
    tagList(
      column(width = 8,
             column(4,
                    pickerInput("hcw_job_role_select", label = "Job Role", 
                                choices = hcw_job_roles, 
                                selected = hcw_job_roles,
                                multiple = TRUE, width = "100%", 
                                options = list(`actions-box` = TRUE))
             ),
             
             column(4,
                    pickerInput("hcw_hb_select", label = "Health Board", 
                                choices = hcw_hb, selected = "Scotland",
                                multiple = TRUE,width = "100%",
                                options = list(`actions-box` = TRUE))
             ),
             column(4,
                    br(),
                    prettyCheckbox("hcw_hospital", 
                                   label = strong("Include HCW Hospital/ Organisation"), 
                                   value = 0,
                                   shape = "curve",icon = icon("check"),
                                   animation = "pulse")
             )
      ))
  }
})

output$pc_hcw_tab_main_ui <- renderUI({
  
  
  ## Primary Care
  if(input$pc_hcw_radio_select == "Primary Care Workers"){
    tagList(
      
      p(pc_completeness_lab),
      plotOutput("pc_comp_chart",width = "500px", height ="70px"),
      
      br(),
      h4("Cumulative Number of tests by Job Role for Primary Care Workers"),
      dataTableOutput("pc_worker_table")
    )
  }else
    
    ## Healthcare Workers
    if(input$pc_hcw_radio_select == "Healthcare Workers"){
      tagList(
        
        p(hcw_completeness_lab),
        plotOutput("hcw_comp_chart",width = "500px", height ="70px"),
        
        br(),
        h4("Cumulative Number of tests by Job Role for Healthcare Workers"),
        dataTableOutput("hcw_worker_table")
      )
    }
  
})

### Reactive Data ----
hcw_test_reasons_format_reac <- reactive({
  if(input$hcw_hospital == 0){
    hcw_test_reasons_format %>% 
      filter(`Job Role` %in% input$hcw_job_role_select,
             `Health Board` %in% input$hcw_hb_select)
  }else{
    hcw_test_reasons_hosp_format %>% 
      filter(`Job Role` %in% input$hcw_job_role_select,
             `Health Board` %in% input$hcw_hb_select)
  }
})


### Render Outputs ----
output$pc_comp_chart <- renderPlot({completeness_bar(c = pc_completeness)})
output$hcw_comp_chart <- renderPlot({  completeness_bar(c = hcw_completeness)})
output$pc_worker_table <- renderDataTable({build_pc_hcw_tables(pc_test_reasons_format)})
output$hcw_worker_table <- renderDataTable({build_pc_hcw_tables(hcw_test_reasons_format_reac(), dom = "tipB", nrow = 20)}, server = FALSE)