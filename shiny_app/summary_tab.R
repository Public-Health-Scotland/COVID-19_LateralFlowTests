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

observeEvent(input$Profession_select, {

  data <- LFT_hb %>%
    filter(test_cohort_name == input$Profession_select)

  # create new lists based on filter selected
  location_update <- c((sort(unique(data$Health_Board_Name)))) # Practices needs to be blank to allow typing

  # update filters
  updatePickerInput(session,
                    "Location_select",
                    choices = location_update, 
                    selected = location_update)
})

############################################### Reactive Charts ###############################################
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({
  
  tagList(h3("Daily number of tests by result"),
          plot_box("Daily number of tests by result", plot_output = "results_overall"),
          
          h3("Number of tests by NHS Board"),
          plot_box("Number of tests by NHS Board", plot_output = "results_location"),
          
          h3("Number of tests per individual by work location in latest week"),
          plot_box("Number of tests per individual by work location", plot_output = "testnumbers_chart"), 
          
          h3("Number of tests per individual by work location in latest rolling four week period"),
          plot_box("Number of tests per individual by work location", plot_output = "testnumbers_chart_roll"))

 }) # end of render UI


############################################### Charts ###############################################

# Creating plots for each cut and dataset
output$results_overall <- renderPlotly({plot_overall_chart(LFT_hb, data_name = "LFT_hb")})
output$results_location <- renderPlotly({plot_location_chart(LFT_hb, data_name = "LFT_hb")})
output$testnumbers_chart <- renderPlotly({plot_testnumbers_chart(TestNumbersChart, data_name = "TestNumbersChart")})
output$testnumbers_chart_roll <- renderPlotly({plot_testnumbers_chart_roll(TestNumbersChartRoll, data_name = "TestNumbersChartRoll")})

