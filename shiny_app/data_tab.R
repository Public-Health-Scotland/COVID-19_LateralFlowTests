

## Reactive data ----

##reactive data to show in app
data_table <- reactive({  # Change dataset depending on what user selected
  
  table_data <- switch(input$data_select,
                       "LabCases" = LabCases %>%  rename (`Number of Cases` = Count, 
                                                          `Cumulative Cases` = Cumulative,
                                                          `7 day average` = Average7, 
                                                          `Cumulative Rate per 100,000` = CumulativeRatePer100000)) 
  
  if (input$data_select %in% c("LabData")) {
    table_data <- table_data %>% 
      select(Date, `Number of Daily Cases`, Cumulative, `Cumulative Rate per 100,000`) %>% 
      mutate(Date = format(Date, "%d %B %y"))

   } #else if (input$data_select %in% "Admissions") { 
  #   table_data <- table_data %>%
  #     select(Date, `Number of Admissions`, `7 day average`) 
#  } 
  
table_data %>% 
    mutate_if(is.numeric, round, 1) %>% 
    mutate_if(is.character, as.factor)
})

###############################################.
## Table ----

output$table_filtered <- DT::renderDataTable({
  
  # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(data_table()))

  DT::datatable(data_table(), style = 'bootstrap',
                class = 'table-bordered table-condensed',
                rownames = FALSE,
                options = list(pageLength = 20,
                               dom = 'tip',
                               autoWidth = TRUE),
                filter = "top",
                colnames = table_colnames)
})

###############################################.
## Data downloads ----

# Data download of data table. 
output$download_table_csv <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
  } 
)
