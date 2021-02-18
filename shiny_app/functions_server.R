# Functions for server side

# Function that creates line trend charts in Plotly for different splits: age, sex, SIMD 
# Three parameters: pal_chose - what palette of colours you want
# dataset - what data to use for the chart formatted as required

############################################### Function for overall charts ###############################################
#output$results_overall <- renderPlotly({plot_overall_chart(tidyLFT, data_name = "tidyLFT")})

plot_overall_chart <- function(dataset, data_name, yaxis_title, area = T) {
  
  # Filtering dataset to include only overall figures
  trend_data <- tidyLFT %>% 
    group_by(test_cohort_name, test_result, new_date) %>% 
    summarise(Count = sum(Count)) %>% 
    spread(test_result, Count) %>%
    filter(test_cohort_name %in% input$Profession_select)
  
  trend_data[is.na(trend_data)] <- 0
  
  # Creating objects that change depending on dataset
  yaxis_title <- case_when(data_name == "tidyLFT" ~ "Daily number of tests by result")
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
 # measure_name <- case_when(data_name == "tidyLFT" ~ "Daily number of tests")
  
  #Text for tooltip
  tooltip_trend <- c(paste0("Date: ", format(trend_data$new_date, "%d %b %y"),
                            "<br>", "Number of positive tests: ", trend_data$POSITIVE,
                            "<br>", "Number of inconclusive tests: ", trend_data$INCONCLUSIVE,
                            "<br>", "Number of negative tests: ", trend_data$NEGATIVE))
  
  
  #Creating time trend plot
  plot_ly(data = trend_data, x = ~new_date) %>%
    add_lines(y = ~POSITIVE, line = list(color = "#000000"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Positive") %>%
    add_lines(y = ~INCONCLUSIVE, line = list(color = "#9B4393"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Inconclusive") %>%
    add_lines(y = ~NEGATIVE, line = list(color = "#0078D4"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Negative") %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

################################################ Location stacked bar chart ###############################################
# cases stacked bar chart
plot_location_chart <- function(dataset, data_name, settingdata, yaxis_title, area = T) {

  trend_data <- tidyLFT %>% 
    filter(test_cohort_name %in% input$Profession_select) %>% 
    filter(Health_Board_Name %in% input$Location_select)  %>%     
    group_by(test_cohort_name, Health_Board_Name, test_result) %>% 
    summarise(Count = sum(Count)) 

  yaxis_title <-  "Number of Tests"

  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title

  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}<br>",
                        "{trend_data$Health_Board_Name}<br>",
                        "Number of {trend_data$test_result} tests: {trend_data$Count}")

  # Creating contact tracing time
  trend_data %>%
    plot_ly(x = ~Health_Board_Name, y = ~Count) %>%
    add_bars(color = ~ test_result, #colour group
             colors = pal_tests, #palette
             stroke = I("black"), #outline
             text = tooltip_trend,
             hoverinfo = "text",
             name = ~ test_result) %>%
    # Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}

################################################ Location stacked bar chart ###############################################
# cases stacked bar chart
plot_testnumbers_chart <- function(dataset, data_name, settingdata, yaxis_title, area = T) {
  
  trend_data <- TestNumbersChart %>% 
    filter(test_cohort_name %in% input$Profession_select) %>%  
    filter(Health_Board_Name %in% input$Location_select) %>%
    filter(week_ending != max(week_ending)) %>% 
    filter(week_ending == max(week_ending))

  yaxis_title <-  "Number of Individuals"
  
  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}<br>",
                        "{trend_data$Health_Board_Name}",
                        "Number of individuals who had {trend_data$Numberoftests} tests in week ending {trend_data$week_ending}: {trend_data$Count}<br>")
  
  # Creating contact tracing time
  trend_data %>%
    plot_ly(x = ~Health_Board_Name, y = ~Count) %>%
    add_bars(color = ~ Numberoftests, #colour group
             colors = pal_n_tests, #palette
             stroke = I("black"), #outline
             text = tooltip_trend,
             hoverinfo = "text",
             name = ~ Numberoftests) %>%
    # Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}
###############################################

################################################ Location stacked bar chart ###############################################
# cases stacked bar chart
plot_testnumbers_chart_roll <- function(dataset, data_name, settingdata, yaxis_title, area = T) {
  
  trend_data <- TestNumbersChartRoll %>% 
    filter(test_cohort_name %in% input$Profession_select) %>%  
    filter(Health_Board_Name %in% input$Location_select) %>%
    filter(roll_week_ending != max(roll_week_ending)) %>% 
    filter(roll_week_ending == max(roll_week_ending))
  
  yaxis_title <-  "Number of Individuals"
  
  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}<br>",
                        "{trend_data$Health_Board_Name}",
                        "Number of individuals who had {trend_data$Numberoftests} tests in week ending {trend_data$roll_week_ending}: {trend_data$Count}<br>")
  
  # Creating contact tracing time
  trend_data %>%
    plot_ly(x = ~Health_Board_Name, y = ~Count) %>%
    add_bars(color = ~ Numberoftests, #colour group
             colors = pal_n_tests, #palette
             stroke = I("black"), #outline
             text = tooltip_trend,
             hoverinfo = "text",
             name = ~ Numberoftests) %>%
    # Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}
############################################### END