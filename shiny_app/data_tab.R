# ############################################### Reactive data ###############################################
##reactive data to show in app
data_table <- reactive({  # Change dataset depending on what user selected
  
  table_data <- switch(input$data_select,
                       "tidyLFT" = tidyLFT %>% rename (`Profession` = test_cohort_name, 
                                                       `Work location type` = LocationType,
                                                       `Work location name` = LocationName, 
                                                       `NHS Board` = Health_Board_Name, 
                                                       `Test result` = test_result, 
                                                       `Date` = new_date),
                       "TestNumbers" = TestNumbers %>%  rename (`Week ending` = week_ending, 
                                                                `Profession` = test_cohort_name,
                                                                `Work location type` = LocationType,
                                                                `Work location name` = LocationName,
                                                                `NHS Board` = Health_Board_Name, 
                                                                `Number of tests` = Number_of_tests,
                                                                `Number of Individuals` = Count), 
                       "TestNumbersRoll" = TestNumbersRoll %>%  rename (`Week ending` = roll_week_ending, 
                                                                        `Profession` = test_cohort_name,
                                                                        `Work location type` = LocationType,
                                                                        `Work location name` = LocationName,
                                                                        `NHS Board` = Health_Board_Name, 
                                                                        `Number of tests` = Number_of_tests,
                                                                        `Number of Individuals` = Count))
  
    
  if (input$data_select %in% c("tidyLFT")) {
    table_data <- table_data %>% 
      select(Date,`Profession`, `Work location type`, `Work location name`, 
             `NHS Board`, `Test result`, Count) 
    
    }   else if (input$data_select %in% "TestNumbers") {
      
      table_data <- table_data
      
    }  else if (input$data_select %in% "TestNumbersRoll") {
      
    table_data <- table_data
  } 
  
  table_data %>% 
    mutate_if(is.numeric, round, 1) %>% 
    mutate_if(is.character, as.factor)
  
  
})


#####################################################################
## DataTable Function
build_data_table_data_tb <- function(data, dom_elements = "tB",nrow=NULL, ...){
  datatable(data, 
            style = "bootstrap",
            extensions = 'Buttons',
            options = list(dom = dom_elements,
                           pageLength = nrow,
                           columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(data)-1)),
                                             list(className = 'dt-left', targets = 0)),
                           initComplete = JS(paste0("function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#433684', 'color': '#ffffff'});}"))),
            rownames = FALSE,
            class = "table-bordered table-hover", ...)
}



############################################### Table ###############################################

output$table_filtered <- DT::renderDataTable({
  
 # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(data_table()))
  
  build_data_table_data_tb(data_table(), filter= "top",
                   dom = "tip", colnames = table_colnames)
  
  }) # end of output

############################################### Data downloads ###############################################

# Data download of data table. 
output$download_table_csv <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
  } 
)