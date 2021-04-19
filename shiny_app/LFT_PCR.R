###############################################  Update filters ############################################### 

observeEvent(input$LFT_PCR_profession_select, {
  
  data <- lft_pcr %>%
    filter(`Test Group` == input$LFT_PCR_profession_select)
  
  # create new lists based on filter selected
  hb_update <- c((sort(unique(data$`NHS Board of Employment`)))) # Practices needs to be blank to allow typing
  
  # update filters
  updatePickerInput(session,
                    "LFT_PCR_hb_select",
                    choices = hb_update, 
                    selected = hb_update)
})

LFT_PCR_data_table <- reactive({ 
  
  lft_pcr %>% 
    filter(`Test Group` %in% input$LFT_PCR_profession_select, 
           `NHS Board of Employment` %in% input$LFT_PCR_hb_select)
  
})


############################################### Table ###############################################

output$LFT_PCR_table_filtered <- DT::renderDataTable({
  
  # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(LFT_PCR_data_table()))
  
  DT::datatable(LFT_PCR_data_table(), style = 'bootstrap',
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
output$LFT_PCR_download_table_csv <- downloadHandler(
  filename ="LFT_PCR_data_extract.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(LFT_PCR_data_table()[input[["LFT_PCR_table_filtered_rows_all"]], ], file) 
  } 
)