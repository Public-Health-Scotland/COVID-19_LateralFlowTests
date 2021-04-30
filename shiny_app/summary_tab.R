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

observeEvent(input$Profession_select, {

  data <- tidyLFT %>%
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
  
  tagList(h3("Weekly number of tests by result"),
          plot_box("Weekly number of tests by result", plot_output = "results_overall"),
          
          h3("Weekly test positivity rate"),
          plot_box("Weekly positivity rate", plot_output = "results_positive"),
          
          h3("Number of tests by NHS Board"),
          plot_box("Number of tests by NHS Board", plot_output = "results_location"),
          
          h3("Number of tests per individual by work location in latest week"),
          plot_box("Number of tests per individual by work location", plot_output = "testnumbers_chart"), 
          
          h3("Number of tests per individual by work location in latest rolling four week period"),
          plot_box("Number of tests per individual by work location", plot_output = "testnumbers_chart_roll"))

 }) # end of render UI


############################################### Charts ###############################################

# Creating plots for each cut and dataset
output$results_overall <- renderPlotly({plot_overall_chart(weekly_chart_complete)})
output$results_positive <- renderPlotly({plot_positivity_chat(PosRate)})
output$results_location <- renderPlotly({plot_location_chart(LFT_hb)})
output$testnumbers_chart <- renderPlotly({plot_testnumbers_chart(TestNumbersChart)})
output$testnumbers_chart_roll <- renderPlotly({plot_testnumbers_chart_roll(TestNumbersChartRoll)})

