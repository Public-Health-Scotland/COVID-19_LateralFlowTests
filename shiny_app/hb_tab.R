############################################### Modals ###############################################

# modal to describe dataset
# Link action button click to modal launch 
observeEvent(input$btn_dataset_modal, 
             
             showModal(modalDialog(
               title = "What is the data source?",
               p("ECOSS (Electronic Communication of Surveillance in Scotland) Database"),
               p(glue("Date extracted: {as.character(dates[1,3])}")),
               p("For a small number of laboratory results initially reported as positive on 
                   subsequent additional testing the laboratory result may be amended to negative,
                   and the individual no longer managed as a confirmed case."),    
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
) # end of observe event for modal


###############################################  Update filters ############################################### 

observeEvent(input$Time_select, {
  
  data <- LFT_hb %>%
    filter(time_period == input$Time_select)
  
})

# observeEvent(input$LFT_profession_select, {
#   
#   data <- LFT_hb %>%
#     filter(test_cohort_name == input$LFT_profession_select)
#   
#   # create new lists based on filter selected
#   NHS_board_select_update <- c((sort(unique(data$Health_Board_Name)))) # Practices needs to be blank to allow typing
#   
#   # update filters
#   updatePickerInput(session,
#                     "NHS_board_select",
#                     choices = NHS_board_select_update, 
#                     selected = NHS_board_select_update)
#   
# })

observeEvent(input$LFT_profession_select, {
  
  data <- LFT_sc %>%
    filter(test_cohort_name == input$LFT_profession_select)
  
})

# observeEvent(input$NHS_board_select, {
# 
#   data <- LFT_hb %>%
#     filter(Health_Board_Name == input$NHS_board_select)
# 
# })

LFT_sc_data_table <- reactive({ 
  
  LFT_sc %>% 
    rename(`Profession` = test_cohort_name, 
           `Time` = time_period, 
           `Tests` = n_tests) %>% 
    select(Profession, `Time`, Tests, Count) %>% 
    # filter(Profession %in% input$LFT_profession_select, 
    #        Time %in% input$Time_select, 
    #        `NHS Board` %in% input$NHS_board_select) %>%
    filter(Profession %in% input$LFT_profession_select, 
           Time %in% input$Time_select) %>% 
    mutate_if(is.numeric, round, 1) %>% 
    mutate_if(is.character, as.factor)
  
})

LFT_hb_data_table <- reactive({ 
  
  LFT_hb %>% 
    rename(`Profession` = test_cohort_name, 
            `NHS Board` = Health_Board_Name,
            `Time` = time_period, 
           `Number of Positive` = Positive, 
           `Number of LFT` = Total, 
           `Percentage Positive` = pc_positive) %>% 
    select(Profession, `NHS Board`, `Time`, `Number of LFT`, 
           `Number of Positive`, `Percentage Positive`) %>% 
    filter(Profession %in% input$LFT_profession_select, 
           Time %in% input$Time_select) %>% 
    mutate_if(is.numeric, round, 1) %>% 
    mutate_if(is.character, as.factor)
  
})

############################################### Table ###############################################

### Functions to build the tables
build_hbtab_data_table <- function(data, dom_elements = "tB",nrow=NULL, ...){
  
  ## Table Column Names
  table_colnames  <-  gsub("_", " ", colnames(data))
  
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
                           columnDefs = list(list(className = 'dt-right', targets = 3:(ncol(data)-1)),
                                             list(className = 'dt-left', targets = 0:2)),
                           initComplete = JS(paste0("function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#433684', 'color': '#ffffff'});}"))),
            rownames = FALSE, colnames = table_colnames,
            class = "table-bordered table-hover", ...)
}



output$LFT_sc_table_filtered <- renderDataTable({ 
  build_hbtab_data_table(LFT_sc_data_table()) 
})

output$LFT_hb_table_filtered <- renderDataTable({
  build_hbtab_data_table(LFT_hb_data_table(), 
                         dom = "tipB", filter = "top", nrow = 15) 
}, server = FALSE)