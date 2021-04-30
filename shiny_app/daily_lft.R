##### Daily LFT Report Tab 
## functions ----

inc_table <- function(i, note = NULL){
  if(lft_table_list[i] %in% input$lft_table_select){
    tagList(
      br(),
      h4(lft_table_list[i]),
      dataTableOutput(paste0("LFT_table_",i)),
      
      if(!is.null(note)){p(tags$i(note))},
      br()
      )
    }
}

build_data_table <- function(data){
  datatable(data, 
            style = "bootstrap",
            extensions = 'Buttons',
            options = list(dom = "tB",
                           pageLength = nrow(data),
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
            class = "table-bordered table-hover")
}

## create reactive data based on the users choices ----
table_1_reac <- reactive({
  table_1 %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) %>%
    set_colnames(c("COVID-19", 
                   str_c("Notifying on ", format(input$lft_date_select - 1, "%d/%m/%Y")," at 8am (since previous return)"), 
                   str_c("Notifying on ", format(input$lft_date_select, "%d/%m/%Y"), " at 8am (since previous return)"), 
                   str_c("Cumulative Total as of ", format(input$lft_date_select, "%d/%m/%Y"))))
  
})

table_2_reac <- reactive({
  table_2 %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) %>%
    set_colnames(c("COVID-19",
                   str_c("Cumulative Total as of ", format(input$lft_date_select, "%d/%m/%Y"))))
})

table_3_reac <- reactive({
  table_3 %>%
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day)
})

table_5_reac <- reactive({
  table_5 %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) %>%
    set_colnames(c("COVID-19", 
                   str_c("Notifying on ", format(input$lft_date_select - 1, "%d/%m/%Y")," at 8am (since previous return)"), 
                   str_c("Notifying on ", format(input$lft_date_select, "%d/%m/%Y"), " at 8am (since previous return)"), 
                   str_c("Cumulative Total as of ", format(input$lft_date_select, "%d/%m/%Y"))))
  
})

table_6a_reac <- reactive({
  table_6a %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) %>%
    set_colnames(c("Category",
                   str_c("Notifying on ",format(input$lft_date_select-1, "%d/%m/%Y")," at 8am (since previous return)"),
                   str_c("Notifying on ",format(input$lft_date_select, "%d/%m/%Y")," at 8am (since previous return)"),
                   str_c("Cumulative Total as of ",format(input$lft_date_select, "%d/%m/%Y"))))
  
})

table_6b_reac <- reactive({
  table_6b %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) 
})

table_7_reac <- reactive({
  table_7 %>% 
    filter(as_date(day) == input$lft_date_select) %>%
    select(-day) 
})


## Key points ----
lft_kp_1 <- reactive({p("On", paste0(input$lft_date_select, ","), strong(table_1_reac()[3,3]),
                        "LFD tests in NHS & UK Government sites were newly reported,",
                        "bringing the cumulative total to",strong(table_1_reac()[3,4], .noWS = "after"), ".")})
lft_kp_2 <- reactive({p("AS of", paste0(input$lft_date_select, ","), strong(table_2_reac()[3,2]),
                        "individuals have been recorded as being tested with an LFD",
                        "at an NHS or UK Government testing site.")})
lft_kp_3 <- reactive({p("On", paste0(input$lft_date_select, ","), strong(table_5_reac()[3,3]),
                        "LFD tests were newly reported via the NSS Portal,",
                        "bringing the cumulative total to",strong(table_5_reac()[3,4], .noWS = "after"), ".")})


## need to build UI from user inputs ----
output$daily_lft_tab <- renderUI({
  
  tagList(
    h3("Key Points"),
    tags$ul(
      tags$li(lft_kp_1()),
      tags$li(lft_kp_2()),
      tags$li(lft_kp_3())
    ),
    br(),
    if(any(lft_table_list[1:3] %in% input$lft_table_select)){
      tagList(
        h3("UK Government Data"),
        p("The source of these data is the", 
          tags$a("NHS Digital platform,", href = "https://www.gov.uk/report-covid19-result", target = "blank_"), 
          "which launched a service for online reporting of self testing results from lateral flow devices (LFDs) on 4 December 2020 and then delivered enhancements to enable its use by different groups. 
          Scottish workers submit an average of", uk_gov_av$n_av, 
          "entries each day (8am to 8am), with a daily maximum of", paste0(uk_gov_av$n_max,".")),
        
        inc_table(1),
        inc_table(2, note = paste("*Figures relate to individuals",
                                  "who have a valid CHI for all test results.",
                                  "Tests without valid CHI are excluded.")),
        inc_table(3)
      )},
    if(any(lft_table_list[4:7] %in% input$lft_table_select)){
      tagList(
        h3("NSS Portal Data"),
        p("The source of these data is the ", tags$a("Scottish Government Lateral Flow Device Portal",
                                                     href ="https://nhsnss.service-now.com/covidtesting", target = "blank_"),
          " where Scottish health and social care staff voluntarily enter their information and Lateral Flow Device test results after taking a test. On average,", 
          nss_portal_av$n_av, "entries are made each day, with a daily maximum of", paste0(nss_portal_av$n_max, ".")),
        
        inc_table(4, note = paste("The NSS Portal data is dynamic and subject to change",
                                  "with each new LFD programme roll-out. Test results",
                                  "are submitted via the NSS Portal app where users",
                                  "manually input their test results.")),
        inc_table(5),
        inc_table(6, note = paste("*Figures relate to individuals",
                                  "who have a valid CHI for all test results.",
                                  "Tests without valid CHI are excluded.")),
        inc_table(7, note = paste("*Figures relate to individuals",
                                  "who have a valid CHI for all test results.",
                                  "Tests without valid CHI are excluded."))
      )}
  )
})


## Render UI ----
output$LFT_table_1 <- renderDataTable({build_data_table(table_1_reac())})
output$LFT_table_2 <- renderDataTable({build_data_table(table_2_reac())})
output$LFT_table_3 <- renderDataTable({build_data_table(table_3_reac())})
output$LFT_table_4 <- renderDataTable({build_data_table(table_5_reac())})
output$LFT_table_5 <- renderDataTable({build_data_table(table_6a_reac())})
output$LFT_table_6 <- renderDataTable({build_data_table(table_6b_reac())})
output$LFT_table_7 <- renderDataTable({build_data_table(table_7_reac())})


## RMarkdown ----
output$lft_report_render_dwnl <- downloadHandler(
  # Output name
  filename = function() { paste0(input$lft_date_select,"_Daily LFT Report.docx") },
  content = function(file) {
    
    # parameters to be used by RMD document
    params <- list(date = input$lft_date_select, tables = input$lft_table_select,
                   table_1_reac = table_1_reac(),table_2_reac = table_2_reac(),
                   table_3_reac = table_3_reac(),table_5_reac = table_5_reac(),
                   table_6a_reac = table_6a_reac(),table_6b_reac = table_6b_reac(), 
                   table_7_reac = table_7_reac(),
                   lft_kp_1 = lft_kp_1(), lft_kp_2 = lft_kp_2(), lft_kp_3 = lft_kp_3())
    
    # get file paths of our RMD script and reference word doc
    src <- normalizePath(c('rmd scripts/Daily LFT Report.Rmd', 
                           'rmd scripts/NATIONAL_STATS_REPORT_TEMPLATE.docx')) 
    
    # set the working directory to the temp directory
    owd <- setwd(tempdir())
    
    # once function has completed running, set the working dir back where it was
    on.exit(setwd(owd))
    
    # render the report
    file.copy(src, c('Daily LFT Report.Rmd', 
                     'NATIONAL_STATS_REPORT_TEMPLATE.docx'), 
              overwrite = TRUE) 
    
    out <- rmarkdown::render('Daily LFT Report.Rmd',
                             params = params,
                             envir = new.env(parent = globalenv()))
    
    file.rename(out, file)
  }
  
)

# #### Excel Tables ----
output$lft_excel_dwnl <- downloadHandler(
  # Output
  filename = function() { paste0(input$lft_date_select,"_LFT Tables.xlsx") },
  content = function(file) {
    
    # set the working directory to the temp directory
    owd <- setwd(tempdir())
    
    # once function has completed running, set the working dir back where it was
    on.exit(setwd(owd))
    
    # put data in list
    index <- which(lft_table_list %in% input$lft_table_select)
    
    data <- list("UK Gov LFD Tests" = table_1_reac(),
                 "UK Gov LFD Individual" = table_2_reac(),
                 "UK Gov LFDs by HB" = table_3_reac(),
                 "NSS LFD Tests" = table_5_reac(),
                 "NSS LFD by Category" = table_6a_reac(),
                 "NSS LFD Individuals by Category" = table_6b_reac(),
                 "NSS LFD Individuals HB" = table_7_reac())
    
    data <- data[index]
    
    out <- write_xlsx(data,file, format_headers = TRUE)
    
    file.rename(out, file)
  }
  
)
