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


############################################### Reactive Charts ###############################################
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({
  
  tagList(h3("Daily number of tests by result"),
          plot_box("Daily number of tests by result", plot_output = "results_overall"),
          
          h3("Number of tests by work location"),
          plot_box("Number of tests by work location", plot_output = "results_location"))#,
  
 }) # end of render UI


############################################### Charts ###############################################

# Creating plots for each cut and dataset
output$results_overall <- renderPlotly({plot_overall_chart(tidyLFT, data_name = "tidyLFT")})
output$results_location <- renderPlotly({plot_location_chart(tidyLFT, data_name = "tidyLFT")})


############################################### Data downloads ###############################################

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download


output$download_chart_data <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write_csv(tidyLFT(),
              file) })
