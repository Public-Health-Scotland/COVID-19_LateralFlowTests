#####################################################################
## DataTable Function
build_data_table <- function(data, dom_elements = "tB",nrow=NULL, ...){
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

# output$LFT_PCR_table_filtered <- DT::renderDataTable({
#   
#   # Remove the underscore from column names in the table
#   table_colnames  <-  gsub("_", " ", colnames(LFT_PCR_data_table()))
#   
#   DT::datatable(LFT_PCR_data_table(), style = 'bootstrap',
#                 class = 'table-bordered table-condensed',
#                 rownames = FALSE,
#                 options = list(pageLength = 20,
#                                dom = 'tip',
#                                autoWidth = TRUE),
#                 filter = "top",
#                 colnames = table_colnames)
# }) # end of output

output$LFT_PCR_table_filtered <- renderDataTable({
  table_colnames  <-  gsub("_", " ", colnames(LFT_PCR_data_table()))
  
  build_data_table(LFT_PCR_data_table(), filter= "top",
                   dom = "tipB", colnames = table_colnames)
  
}, server = FALSE)

