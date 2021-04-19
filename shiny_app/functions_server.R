# Functions for server side

# Function that creates line trend charts in Plotly for different splits: age, sex, SIMD 
# Three parameters: pal_chose - what palette of colours you want
# dataset - what data to use for the chart formatted as required

############################################### Function for overall charts ###############################################
#output$results_overall <- renderPlotly({plot_overall_chart(tidyLFT, data_name = "tidyLFT")})

plot_overall_chart <- function(dataset, area = T) {
  
  # Filtering dataset to include only overall figures
  trend_data <- weekly_chart_complete %>% 
    # full_join(tidyLFT_expanded_df) %>% 
    group_by(test_cohort_name, Health_Board_Name, test_result, week_ending) %>% 
    summarise(Count = sum(Count)) %>% 
    spread(test_result, Count) %>% 
    filter(test_cohort_name %in% input$Profession_select) %>%
    filter(Health_Board_Name %in% input$Location_select)
  
  trend_data[is.na(trend_data)] <- 0
  
  # Creating objects that change depending on dataset
  yaxis_title <- "Weekly number of tests by result"
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
 # measure_name <- case_when(data_name == "tidyLFT" ~ "Daily number of tests")
  
  #Text for tooltip
  tooltip_trend <- c(paste0(trend_data$Health_Board_Name, 
                            "<br>", "Date: ", format(trend_data$week_ending, "%d %b %y"),
                            "<br>", "Number of positive tests: ", trend_data$Positive,
                            "<br>", "Number of inconclusive tests: ", trend_data$Inconclusive,
                            "<br>", "Number of insufficient tests: ", trend_data$Insufficient,
                            "<br>", "Number of negative tests: ", trend_data$Negative))
  
  
  #Creating time trend plot
  plot_ly(data = trend_data, x = ~week_ending) %>%
    add_lines(y = ~Positive, line = list(color = "#000000"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Positive") %>%
    add_lines(y = ~Inconclusive, line = list(color = "#9B4393"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Inconclusive") %>%
    add_lines(y = ~Insufficient, line = list(color = "#9b4347"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Insufficient") %>%
    add_lines(y = ~Negative, line = list(color = "#0078D4"),
              text = tooltip_trend, hoverinfo = "text",
              name = "Negative") %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

############################################### Positivity chart ###############################################
plot_positivity_chat <- function(dataset, area = T) {

  # Filtering dataset to include only overall figures
  pos_data <- dataset %>%
    select(test_cohort_name, Health_Board_Name, week_ending, positive_pc) %>%
    filter(test_cohort_name %in% input$Profession_select) %>%
    filter(Health_Board_Name %in% input$Location_select)

  pos_data[is.na(pos_data)] <- 0

  # Creating objects that change depending on dataset
  yaxis_title <- "Weekly Positivity Percentage"
  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title

  # measure_name <- case_when(data_name == "tidyLFT" ~ "Daily number of tests")

  #Text for tooltip
  tooltip_trend <- c(paste0(pos_data$Health_Board_Name,
                            "<br>", "Date: ", format(pos_data$week_ending, "%d %b %y"),
                            "<br>", "Percentage Positive Tests ", pos_data$positive_pc))


  #Creating time trend plot
  plot_ly(data = pos_data, x = ~week_ending) %>%
    add_lines(y = ~positive_pc, group = ~Health_Board_Name, 
              color = ~Health_Board_Name, colors = pal_pos, #palette
              text = tooltip_trend, hoverinfo = "text") %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}

################################################ Location stacked bar chart ###############################################
# cases stacked bar chart
plot_location_chart <- function(dataset, area = T) {

  trend_data <- tidyLFT %>% 
    filter(test_cohort_name %in% input$Profession_select) %>% 
    filter(Health_Board_Name %in% input$Location_select)  %>%     
    group_by(test_cohort_name, Health_Board_Name, test_result) %>% 
    summarise(Count = sum(Count)) 

  yaxis_title <- "Number of Tests"

  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title

  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}",
                        "<br>", "{trend_data$Health_Board_Name}",
                        "<br>", "Number of {trend_data$test_result} tests: {trend_data$Count}")

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
plot_testnumbers_chart <- function(dataset, area = T) {
  
  trend_data <- dataset %>% 
    filter(test_cohort_name %in% input$Profession_select) %>%  
    filter(Health_Board_Name %in% input$Location_select)

  yaxis_title <- "Number of Individuals"
  
  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}",
                        "<br>", "{trend_data$Health_Board_Name}",
                        "<br>", "Number of individuals who had {trend_data$Numberoftests} tests in week ending {trend_data$week_ending}: {trend_data$Count}<br>")
  
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
plot_testnumbers_chart_roll <- function(dataset, area = T) {
  
  trend_data <- dataset %>% 
    filter(test_cohort_name %in% input$Profession_select) %>%  
    filter(Health_Board_Name %in% input$Location_select)
  
  yaxis_title <- "Number of Individuals"
  
  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  # ext for tooltip
  tooltip_trend <- glue("{trend_data$test_cohort_name}",
                        "<br>", "{trend_data$Health_Board_Name}",
                        "<br>", "Number of individuals who had {trend_data$Numberoftests} tests in week ending {trend_data$roll_week_ending}: {trend_data$Count}<br>")
  
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