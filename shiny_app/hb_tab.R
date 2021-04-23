############################################### Modals ###############################################

# modal to describe dataset
# Link action button click to modal launch 
observeEvent(input$btn_dataset_modal, 
             
             showModal(modalDialog(
               title = "What is the data source?",
               p("Name of Data source goes here"),
               p(glue("Date extracted: ")),
               p("Text goes here") ,    
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

output$LFT_sc_table_filtered <- DT::renderDataTable({
  
  # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(LFT_sc_data_table()))
  
  DT::datatable(LFT_sc_data_table(), style = 'bootstrap',
                class = 'table-bordered table-condensed',
                rownames = FALSE,
                options = list(pageLength = 20,
                               dom = 'tip',
                               autoWidth = TRUE),
                filter = "top",
                colnames = table_colnames)
}) # end of output

output$LFT_hb_table_filtered <- DT::renderDataTable({
  
  # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(LFT_hb_data_table()))
  
  DT::datatable(LFT_hb_data_table(), style = 'bootstrap',
                class = 'table-bordered table-condensed',
                rownames = FALSE,
                options = list(pageLength = 20,
                               dom = 'tip',
                               autoWidth = TRUE),
                filter = "top",
                colnames = table_colnames)
}) # end of output


############################################### Data downloads ###############################################

# Data download of data table.
output$LFT_download_table_csv_scot <- downloadHandler(
  filename ="LFT_data_extract_scotland.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(LFT_sc_data_table()[input[["LFT_sc_table_filtered_rows_all"]], ], file)
  }
)

# Data download of data table.
output$LFT_download_table_csv <- downloadHandler(
  filename ="LFT_data_extract.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(LFT_hb_data_table()[input[["LFT_hb_table_filtered_rows_all"]], ], file)
  }
)