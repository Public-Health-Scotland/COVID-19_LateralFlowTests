#### Trend Chart Page
#### Functions ----

update_hb_filter <- function(){
  location_update <- tidyLFT %>%
    filter(test_cohort_name == input$trend_chart_profession_sel) %>%
    filter(Health_Board_Name != "Scotland") %>%
    pull(Health_Board_Name) %>%
    unique() %>% sort
  
  # update filters
  updatePickerInput(session,
                    "trend_chart_hb_select",
                    choices = location_update, 
                    selected = location_update)
}

chart_1 <- function(){
  
  data <- chart_1_data()
  
  tooltip <- c(paste0(data$Health_Board_Name,
                      "<br>", "Date: ", format(data$week_ending, "%d %b %y"),
                      "<br>", "Test Result: ", paste0(input$trend_tab_chart_1_test_result),
                      "<br>", "Number of tests: ", format(data$Count, big.mark = ",")))
  
  ### SCOTLAND CHART
  if(input$trend_chart_geog_select == "Scotland"){
    
    p<-ggplotly(
      ggplot(data, aes(x = week_ending, y = Count)) +
        geom_line(aes(colour = Health_Board_Name, size = Health_Board_Name)) +
        scale_colour_manual(values = "#433684") +
        scale_size_manual(values = 1) +
        scale_y_continuous(labels = function(x) { format(x, big.mark = ",") },
                           limits = c(0,NA)) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.title.x = element_blank()) +
        labs(y = "Weeky Number of Tests"), height = 500)
    
    p$x$data[[1]]$text <- tooltip

  } else {
    
    ### HEALTH BOARD CHART
    
    ### if the highlight hb checkbox is there
    if(length(input$trend_tab_highlight_hb_chkbx) > 0){
      ### if the highlight hb checkbox is checked
    if(input$trend_tab_highlight_hb_chkbx == 1) {
      if(length(input$trend_chart_hb_high) > 0){
        data <- data %>%
          mutate(highlight = if_else(Health_Board_Name == input$trend_chart_hb_high, 1, 0))
        # hb_order <- c(unique(filter(data, Health_Board_Name != input$trend_chart_hb_high)$Health_Board_Name),
        #               input$trend_chart_hb_high)
      }
      if(length(input$trend_chart_hb_select) > 1){
      plot_colours  <- c("#433684", "#7c6ec4" )
      plot_line_size <- c(1, 0.2)
      } else {      
        plot_colours <- "#433684"
        plot_line_size <- 1}
    } else {
      data <- data %>% mutate(highlight = 0)
      #hb_order <- unique(data$Health_Board_Name)
      
      plot_colours <- "#7c6ec4"
      plot_line_size <- 0.2
    }
  
    
    p <- ggplotly(
      ggplot(data, aes(x = week_ending, y = Count, 
                            group = Health_Board_Name, 
                            color = factor(highlight, levels=1:0),
                       label = tooltip)) +
        geom_line(aes(size =factor(highlight, levels=1:0))) +
        scale_size_manual(values = plot_line_size) +
        scale_colour_manual(values = plot_colours) +
        scale_y_continuous(labels = function(x) { format(x, big.mark = ",") },
                           limits = c(0,NA)) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.title.x = element_blank()) +
        labs(y = "Weeky Number of Tests"), height = 500)
    
      p$x$data[[1]]$text <- sub(".*tooltip: ", "", p$x$data[[1]]$text )
      if(sum(data$highlight) > 0 & length(input$trend_chart_hb_select) > 1){
        p$x$data[[2]]$text <- sub(".*tooltip: ", "", p$x$data[[2]]$text )
      }
    }}
  
  p %>% #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
}

chart_2 <- function(){
  
  # heatmap function:
  build_heatmap <- function(data){
    n_weeks <- unique(data$week_ending) %>% length()
    n_tc <- unique(data$Health_Board_Name) %>% length()
    
    tooltip <- c(paste0(data$Health_Board_Name,
                        "<br>", "Date: ", format(data$week_ending, "%d %b %y"),
                        "<br>", "% Positive: ", scales::percent(data$positive_pc,scale = 1,accuracy = 0.01))) %>%
      sub("NA", "0.00%", .)
    
    p <- ggplotly(ggplot(data, 
                         aes(week_ending, Health_Board_Name,fill= positive_pc,
                             label = tooltip)) + 
                    geom_tile() +
                    theme_minimal()+
                    labs(fill = "Percent Positive") +
                    theme(axis.title.y = element_blank(),
                          legend.position = "bottom")+
                    scale_fill_gradient(low = "#ebb7e6", high = "#9B4393",na.value = "lightgrey",
                                        labels = function(x){scales::percent(x, scale = 1)}),
                  
                  height =   if_else(n_tc > 14, 20.8*n_tc-1, 280))
    
    p$x$data[[1]]$text <- sub(".*tooltip: ", "", p$x$data[[1]]$text )
    p
  }
  
  ## Function to build cumulative graphs
  build_bar_graph <- function(data){
    
    data$Health_Board_Name <- factor(data$Health_Board_Name, levels = data$Health_Board_Name)
    tooltip <- paste0(data$Health_Board_Name, "<br>", 
                      "Percent Positive: ",scales::percent(data$positive_pc, accuracy=0.01, scale=1))
    
    p<-ggplotly(
      ggplot(data, aes(x = Health_Board_Name,
                       y = positive_pc)) +
        geom_bar(stat = "identity", position =position_dodge(), fill="#433684") +
        scale_y_continuous(labels = function(x) { scales::percent(x, scale = 1, accuracy = 0.01) }, 
                           limits = c(0, NA)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90, hjust = 1),
              axis.title.x = element_blank()) +
        labs(y = "Percent Positive"), height = 500
    )
    
    p$x$data[[1]]$text <- tooltip
    p
    
  }
  
  data <- chart_2_data()
  
  ## SCOTLAND
  if(input$trend_chart_geog_select == "Scotland"){
    
    
    ## Cumulative
    if(input$trend_tab_chart_2_type == "Cumulative"){
      p <- build_bar_graph(data)
    }
    
    ## Weekly
    if(input$trend_tab_chart_2_type == "Weekly"){
    # tooltip
    tooltip <- c(paste0("Scotland",
                        "<br>", "Date: ", format(data$week_ending, "%d %b %y"),
                        "<br>", "% Positive: ", scales::percent(data$positive_pc,scale = 1,accuracy = 0.01))) %>%
      sub("NA", "0.00%", .)

    p <- ggplotly(ggplot(data, aes(x = week_ending, y = replace_na(positive_pc,0),
                              group = 1, labels = tooltip)) +
               geom_line(color = "#433684", size = 1) +

               scale_y_continuous(labels = function(x) { scales::percent(x, scale = 1, accuracy = 0.01) },
                                  limits = c(0,NA)) +
               theme_minimal() +
               theme(legend.position = "none",
                     axis.title.x = element_blank()) +
               labs(y = "Weeky Percent Positive"), height = 500)
    p$x$data[[1]]$text <- sub(".*tooltip: ", "", p$x$data[[1]]$text )
     # p <- build_heatmap(data)
    }
    p <- p %>% 
      layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) #position of legend
  } ## scotland if statement
  
  #### HEALTH BOARD
  if(input$trend_chart_geog_select == "Health Board"){
    
    ## Cumulative
    if(input$trend_tab_chart_2_type == "Cumulative"){
      
      if(length(data$Health_Board_Name) > 0){
      b_margin <- max(nchar(as.character(data$Health_Board_Name)))*8.3
      } else {
        b_margin <- 80
      }
      b_margin <- if_else(b_margin < 80, 80, b_margin)
      
      p <- build_bar_graph(data) %>%
        layout(margin = list(b = b_margin, t = 5), #to avoid labels getting cut out
               yaxis = yaxis_plots, xaxis = xaxis_plots,
               legend = list(x = 100, y = 0.5)) #position of legend
    }
    
    ## Weekly
    if(input$trend_tab_chart_2_type == "Weekly"){
      p <- build_heatmap(data) %>%
        layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
               yaxis = yaxis_plots, xaxis = xaxis_plots,
               legend = list(x = 100, y = 0.5)) #position of legend
    }
    
  } ## Health Board if statement
  
  p %>% 
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

chart_3 <- function(){
  
  
  data <- chart_3_data()
  
  if(length(data$Health_Board_Name) > 0){
    b_margin <- max(nchar(as.character(data$Health_Board_Name)))*8.3
  } else {
    b_margin <- 80
  }
  b_margin <- if_else(b_margin < 80, 80, b_margin)
  
  data$Health_Board_Name <- factor(data$Health_Board_Name, levels = data$Health_Board_Name)
  tooltip <- paste0(data$Health_Board_Name, "<br>", 
                    "Number of Tests : ",format(data$Count, big.mark = ","))

  p<-ggplotly(
    ggplot(data, aes(x = Health_Board_Name,
                     y = Count)) +
      geom_bar(stat = "identity", position =position_dodge(), fill="#433684") +
      scale_y_continuous(labels = function(x){format(x, big.mark = ",")}, limits = c(0, NA)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=90, hjust = 1),
            axis.title.x = element_blank()) +
      labs(y = "Number of Tests"), height = 500
  )
  
  p$x$data[[1]]$text <- tooltip
  p %>%
    layout(margin = list(b = b_margin, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend 
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
}

chart_4 <- function(){

  data <- chart_4_data()
  
  if(length(data$Health_Board_Name) > 0){
    b_margin <- max(nchar(as.character(data$Health_Board_Name)))*8.3
  } else {
    b_margin <- 80
  }
  b_margin <- if_else(b_margin < 80, 80, b_margin)
  
  bar_mode <- case_when(input$trend_tab_chart_4_type == "Stacked" ~ "stack",
                        input$trend_tab_chart_4_type == "Grouped" ~ "group")
  
  yaxis_title <- "Number of Individuals"

  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title

  # ext for tooltip
  tooltip_trend <- glue("{data$test_cohort_name}",
                        "<br>", "{data$Health_Board_Name}",
                        "<br>", "Number of individuals who had {data$Numberoftests} tests in week ending {data$week_ending}: {data$Count}<br>")

  # Creating contact tracing time
  data %>%
    plot_ly(x = ~Health_Board_Name, y = ~Count, height = 500) %>%
    add_bars(color = ~ Numberoftests, #colour group
             colors = pal_n_tests, #palette
             stroke = I("black"), #outline
             text = tooltip_trend,
             hoverinfo = "text",
             name = ~ Numberoftests) %>%
    # Layout
    layout(margin = list(b = b_margin, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = append(xaxis_plots,list(tickangle = 270)),
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = bar_mode) %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

}

chart_5 <- function(){
  
  data <- chart_5_data()
  
  if(length(data$Health_Board_Name) > 0){
    b_margin <- max(nchar(as.character(data$Health_Board_Name)))*8.3
  } else {
    b_margin <- 80
  }
  b_margin <- if_else(b_margin < 80, 80, b_margin)
  
  bar_mode <- case_when(input$trend_tab_chart_5_type == "Stacked" ~ "stack",
                        input$trend_tab_chart_5_type == "Grouped" ~ "group")
  
  yaxis_title <- "Number of Individuals"
  
  # Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  # ext for tooltip
  tooltip_trend <- glue("{data$test_cohort_name}",
                        "<br>", "{data$Health_Board_Name}",
                        "<br>", "Number of individuals who had {data$Numberoftests} tests in week ending {data$roll_week_ending}: {data$Count}<br>")
  
  # Creating contact tracing time
  data %>%
    plot_ly(x = ~Health_Board_Name, y = ~Count, height = 500) %>%
    add_bars(color = ~ Numberoftests, #colour group
             colors = pal_n_tests, #palette
             stroke = I("black"), #outline
             text = tooltip_trend,
             hoverinfo = "text",
             name = ~ Numberoftests) %>%
    # Layout
    layout(margin = list(b = b_margin, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = append(xaxis_plots,list(tickangle = 270)),
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = bar_mode) %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
}

#### Objects ----

## Normal

## Reactive
chart_1_data <- reactive({
  if(input$trend_chart_geog_select == "Scotland"){
    weekly_chart_complete %>%
      filter(Health_Board_Name == "Scotland",
             test_cohort_name == input$trend_chart_profession_sel)%>% 
      filter(test_result == input$trend_tab_chart_1_test_result)  %>% 
      mutate(Count = replace_na(Count,0)) %>%
      group_by(Health_Board_Name,week_ending) %>% 
      summarise(Count = sum(Count)) %>% ungroup()
    } else {
      weekly_chart_complete %>%
        filter(Health_Board_Name %in% input$trend_chart_hb_select,
               test_cohort_name == input$trend_chart_profession_sel)%>% 
        filter(test_result == input$trend_tab_chart_1_test_result)  %>% 
        mutate(Count = replace_na(Count,0)) %>%
        group_by(Health_Board_Name,week_ending) %>% 
        summarise(Count = sum(Count)) %>% ungroup()
    } 
})

chart_2_data <- reactive({
  
  ## SCOTLAND DATA
  if(input$trend_chart_geog_select == "Scotland"){
    
    ## WEEKLY
    if(input$trend_tab_chart_2_type == "Weekly"){
    PosRate %>%
      filter(Health_Board_Name == "Scotland",
             test_cohort_name == input$trend_chart_profession_sel)%>% 
      select(Health_Board_Name, week_ending, positive_pc) %>%
      mutate(positive_pc = if_else(positive_pc == 0, NA_real_, positive_pc))
      
      ## CUMULATIVE
    } else if(input$trend_tab_chart_2_type == "Cumulative"){
      LFT_hb %>% filter(time_period == "Cumulative", 
                        Health_Board_Name == "Scotland",
                        test_cohort_name == input$trend_chart_profession_sel) %>%
        select(Health_Board_Name, positive_pc = pc_positive)
        
    }
  } else {
  
  ## HEALTH BOARD    
    
    ## WEEKLY
    if(input$trend_tab_chart_2_type == "Weekly"){
    PosRate %>%
      filter(Health_Board_Name %in% input$trend_chart_hb_select,
             test_cohort_name == input$trend_chart_profession_sel) %>% 
      select(Health_Board_Name, week_ending, positive_pc) %>%
      mutate(positive_pc = if_else(positive_pc == 0, NA_real_, positive_pc))

      ## CUMULATIVE
    } else if(input$trend_tab_chart_2_type == "Cumulative"){
      LFT_hb %>% filter(time_period == "Cumulative", 
                        Health_Board_Name %in% input$trend_chart_hb_select,
                        test_cohort_name == input$trend_chart_profession_sel) %>%
        select(Health_Board_Name, positive_pc = pc_positive) %>%
        arrange(desc(positive_pc))
    } 
    }
})

chart_3_data <- reactive({
  data <- cumulative_tests %>%
    
    filter(test_cohort_name == input$trend_chart_profession_sel,
           test_result == input$trend_tab_chart_3_test_result) 
  
  if(input$trend_chart_geog_select == "Scotland"){
    data <- data %>% filter(Health_Board_Name == "Scotland")
  } else if (input$trend_chart_geog_select == "Health Board") {
    data <- data %>% filter(Health_Board_Name %in% input$trend_chart_hb_select)
  }
  data %>%
    arrange(desc(Count))
})

chart_4_data <- reactive({
  data <- TestNumbersChart %>%
    
    filter(test_cohort_name == input$trend_chart_profession_sel) 
  
  if(input$trend_chart_geog_select == "Scotland"){
    data <- data %>% filter(Health_Board_Name == "Scotland")
  } else if (input$trend_chart_geog_select == "Health Board") {
    data <- data %>% filter(Health_Board_Name %in% input$trend_chart_hb_select)
  }
  data %>%
    arrange(desc(Count))
  
})

chart_5_data <- reactive({
  data <- TestNumbersChartRoll %>%
    
    filter(test_cohort_name == input$trend_chart_profession_sel) 
  
  if(input$trend_chart_geog_select == "Scotland"){
    data <- data %>% filter(Health_Board_Name == "Scotland")
  } else if (input$trend_chart_geog_select == "Health Board") {
    data <- data %>% filter(Health_Board_Name %in% input$trend_chart_hb_select)
  }
  data %>%
    arrange(desc(Count))
  
})

#### Build UI ----

## Widget UI
output$trend_chart_tab_main_widget_ui <- renderUI({
    if(input$trend_chart_geog_select == "Health Board"){
      tagList(
      pickerInput("trend_chart_hb_select", 
                  label = "Select Health Board(s)", 
                  choices = NULL, multiple = TRUE, width = "100%",
                  options = list(`actions-box` = TRUE))
      )
    }
})

output$trend_chart_tab_main_panel <- renderUI({
  
  
  ## Chart 1
  tagList(
    fluidRow(style = "height:580px;",
      column(width=3,
             h4("Chart 1 Options"),
             pickerInput("trend_tab_chart_1_test_result",
                         label = strong("Test Result"), 
                         choices = c("Positive",
                                     "Negative",
                                     "Inconclusive",
                                     "Insufficient",
                                     "All"),
                         selected = "All"),
      if(input$trend_chart_geog_select == "Health Board"){
        tagList(
          prettyCheckbox("trend_tab_highlight_hb_chkbx", 
                         label = strong("Highlight a Health Board"), 
                         value = 0,
                         shape = "curve",icon = icon("check"),
                         animation = "pulse"),
          uiOutput("conditional_hb_highlight"))
        }),
      uiOutput("chart_1")
  ),
  
  ## Chart 2
  fluidRow(style = "height:580px;",
    column(width=3,
           h4("Chart 2 Options"),
           pickerInput("trend_tab_chart_2_type",
                       label = strong("Time Period"), 
                       choices = c("Cumulative",
                                   "Weekly"),
                       selected = "Cumulative")
           ),
    uiOutput("chart_2")
  ),
  
  ## Chart 3
  fluidRow(style = "height:580px;",
    column(width=3,
           h4("Chart 3 Options"),
           pickerInput("trend_tab_chart_3_test_result",
                       label = strong("Test Result"), 
                       choices = c("Positive",
                                   "Negative",
                                   "Inconclusive",
                                   "Insufficient",
                                   "All"),
                       selected = "All")
    ),
    uiOutput("chart_3")
  ), # chart 3
  
  ## Chart 4
  fluidRow(style = "height:580px;",
    column(width=3,
           h4("Chart 4 Options"),
           pickerInput("trend_tab_chart_4_type",
                       label = strong("Bar Chart Type"), 
                       choices = c("Grouped",
                                   "Stacked"),
                                   #"Proportionally Stacked"),
                       selected = "Grouped")
    ),
    uiOutput("chart_4")
  ), # chart 4
  
  ## Chart 5
  fluidRow(style = "height:580px;",
    column(width=3,
           h4("Chart 5 Options"),
           pickerInput("trend_tab_chart_5_type",
                       label = strong("Bar Chart Type"), 
                       choices = c("Grouped",
                                   "Stacked"),
                       #"Proportionally Stacked"),
                       selected = "Grouped")
    ),
    uiOutput("chart_5")
  ) # chart 5
  ) # tagList
})

output$conditional_hb_highlight <- renderUI({
if(input$trend_tab_highlight_hb_chkbx == 1){
  pickerInput("trend_chart_hb_high", 
              label = "Highlight Health Board", 
              choices = NULL)
}
})

### Render Chart UIs
output$chart_1 <- renderUI({
  column(width = 9,
         plot_box(paste0("Chart 1: Weekly Number of ", 
                        ifelse(input$trend_tab_chart_1_test_result != "All",
                               input$trend_tab_chart_1_test_result, ""), 
                        " Tests"), 
                  paste0(
                        input$trend_chart_profession_sel,", ", 
                        case_when(input$trend_chart_geog_select == "Scotland" ~ "All Scotland",
                                  input$trend_chart_geog_select == "Health Board" ~ "By Health Board"
                        )),
                  "chart_1_output")
  )
})
output$chart_2 <- renderUI({
  column(width = 9,
         plot_box(paste0("Chart 2: ",input$trend_tab_chart_2_type, " Test Positivity"),
                  paste0(input$trend_chart_profession_sel,", ", 
                         case_when(input$trend_chart_geog_select == "Scotland" ~ "All Scotland",
                                   input$trend_chart_geog_select == "Health Board" ~ "By Health Board"
                         )),
                  "chart_2_output")
  )
})
output$chart_3 <- renderUI({
  column(width = 9,
         plot_box(paste0("Chart 3: Cumulative Numbers of ",
                         ifelse(input$trend_tab_chart_3_test_result != "All",
                                input$trend_tab_chart_3_test_result, ""), 
                         " Tests"), 
                  paste0(
                    input$trend_chart_profession_sel,", ", 
                    case_when(input$trend_chart_geog_select == "Scotland" ~ "All Scotland",
                              input$trend_chart_geog_select == "Health Board" ~ "By Health Board"
                    )),
                  "chart_3_output")
  )
})
output$chart_4 <- renderUI({
  column(width = 9,
         plot_box(paste0("Chart 4: Number of tests per individual in latest week"),
                  paste0(
                    input$trend_chart_profession_sel,", ", 
                    case_when(input$trend_chart_geog_select == "Scotland" ~ "All Scotland",
                              input$trend_chart_geog_select == "Health Board" ~ "By Health Board of Employment"
                    )),
                  "chart_4_output")
  )
})
output$chart_5 <- renderUI({
  column(width = 9,
         plot_box(paste0("Chart 5: Number of tests per individual in latest rolling four week period"),
                  paste0(
                    input$trend_chart_profession_sel,", ", 
                    case_when(input$trend_chart_geog_select == "Scotland" ~ "All Scotland",
                              input$trend_chart_geog_select == "Health Board" ~ "By Health Board of Employment"
                    )),
                  "chart_5_output")
  )
})


### Update Widget Dropdowns ----
observeEvent(input$trend_chart_geog_select, {
  update_hb_filter()
})

observeEvent(input$trend_chart_profession_sel, {
  update_hb_filter()
})

observeEvent(input$trend_tab_highlight_hb_chkbx, {
  updatePickerInput(session,
                    "trend_chart_hb_high",
                    choices = input$trend_chart_hb_select)
})

observeEvent(input$trend_chart_hb_select, {
  if(input$trend_tab_highlight_hb_chkbx == 1){
  updatePickerInput(session,
                    "trend_chart_hb_high",
                    choices = input$trend_chart_hb_select)
  }
})

#### Render Objects ----
output$chart_1_output <- renderPlotly({chart_1()})
output$chart_2_output <- renderPlotly({chart_2()})
output$chart_3_output <- renderPlotly({chart_3()})
output$chart_4_output <- renderPlotly({chart_4()})
output$chart_5_output <- renderPlotly({chart_5()})
