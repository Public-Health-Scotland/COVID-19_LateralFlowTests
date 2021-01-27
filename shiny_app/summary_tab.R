
## Modals ----

#modal to describe dataset
# Link action button click to modal launch 
observeEvent(input$btn_dataset_modal, 
             
             if (input$measure_select == "LabCases") { # Positive Cases MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("ECOSS (Electronic Communication of Surveillance in Scotland) Database"),
                 p(glue("Date extracted: {labcases_extract_date}")),
                 p("For a small number of laboratory results initially reported as positive on 
                   subsequent additional testing the laboratory result may be amended to negative,
                   and the individual no longer managed as a confirmed case.") ,                                   
                 p("Note: Specimen date was not available for historical UK Government Regional 
                   Testing centres data between 15 and 25 April. As a sample date is required 
                   to report in ECOSS these samples were assigned a specimen date in the mid-point
                   within this date range (20 April). Date refers to the date the sample was 
                   received into the PHS Surveillance System."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
               
             } else if (input$measure_select == "Admissions") { #Admissions MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("ECOSS (Electronic Communication of Surveillance in Scotland) and 
                   RAPID (Rapid Preliminary Inpatient Data"), 
                 p(glue("Data are correct as at the time of data extract at 9am on {admission_extract_date}.
                        Data are reviewed and validated on a continuous basis and 
                        so may be subject to change")),
                 p("Note that there may be a time lag with some data for the most recent days 
                   and some of the above figures may change as more data are submitted. 
                   Data now includes any positive cases from NHS Laboratories or 
                   UK Government regional testing sites."),               
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
               
             } 
               )

###############################################.

## Reactive Charts  ---- 
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({
  
  # text for titles of cut charts
  datasettrend <- case_when(input$measure_select == "LabCases" ~ "Positive COVID-19 cases")
  
  # text for titles of cut charts
  dataset <- case_when(input$measure_select == "LabCases" ~ "Positive COVID-19 cases")
  
  start_date <- case_when(input$measure_select == "LabCases" ~ "28 February 2020")
  
  end_date <- case_when(input$measure_select == "LabCases" ~ Labcases_date)
  
  total_title <- glue("Daily number of {datasettrend}")
 
  
  #subheading <- case_when(input$measure_select == "Admissions" ~ "COVID-19 related admissions have been identified as the following: A patient may have tested positive 
   #                                               for COVID-19 14 days prior to admission to hospital, on the day of their admission or during their stay in hospital.")

# data sources
data_source <- case_when(input$measure_select == "LabCases" ~ "ECOSS")



# Functions for Chart Layouts ---------------------------------------------
# Function to create the standard layout for all the different charts/sections
cut_charts <- function(title, source, data_name) {
  tagList(
    h3(title),
    actionButton("btn_dataset_modal", paste0("Data source: ", source), icon = icon('question-circle'))
   
    )
}

#for e.g. ICU admissions where no SIMD data
cut_charts_missing <- function(title, source, data_name) {
  tagList(
    h3(title),
    p(""),
    p(""),
    actionButton("btn_dataset_modal", paste0("Data source: ", source), icon = icon('question-circle')),
    plot_box(paste0(total_title), paste0(data_name, "_overall")))
}

# Set up Charts for each section ------------------------------------------

# Charts and rest of UI
if (input$measure_select == "LabCases") { #Positive Cases
  
  tagList(h3("Daily number of positive COVID-19 cases"),
          actionButton("btn_dataset_modal", paste0("Data source: ", "ECOSS"), icon = icon('question-circle')),
          plot_box("Daily number of Positive COVID-19 cases", plot_output = "LabCases_overall"),
          plot_box("Cumulative rate per 100,000", plot_output = "LabCasesRate"),
          plot_cut_box(paste0("Positive COVID-19 cases per 100,000 population by age \n(28 February 2020 to ", Labcases_date, ")"), "LabCases_AgeSex",
                       paste0("Positive COVID-19 cases by deprivation category (SIMD) \n(28 February to 2020 ", Labcases_date, ")"), "LabCases_SIMD"))
         
} else if (input$measure_select == "Admissions") { #Admissions
  cut_charts_subheading(title= "Daily number of COVID-19 admissions to hospital", 
                        source = data_source, data_name = "Admissions")
  
}


}) 

###############################################.
## Charts ----

# Creating plots for each cut and dataset
# Trend Charts
output$LabCases_overall <- renderPlotly({plot_overall_chart(LabCases, data_name = "LabCases")})


## Data downloads ----


# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
overall_data_download <- reactive({
  switch(
    input$measure_select,
    "LabCases" = LabCases) #%>%
  #select(area_name, week_ending, count, starts_with("average")) %>%
  # mutate(week_ending = format(week_ending, "%d %b %y"))
})

output$download_chart_data <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write_csv(overall_data_download(),
              file) }
)
