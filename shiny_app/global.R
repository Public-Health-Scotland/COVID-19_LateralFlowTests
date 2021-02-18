# Global

############################################### Dates ###############################################

#publication date
pub_date <- as.Date("2021-01-27")


############################################### Packages ###############################################
#library(devtools)
# devtools::install_github("andrewsali/shinycssloaders")
library(shiny)
library(plotly) # for charts
library(tidyverse)
library(shinyWidgets) # for dropdowns
library(DT) # for data table
library(shinyjs) # for enable/disable functions
library(readr) # for writing/reading csvs 
library(stringr) #for manipulating strings
library(forcats) #manipulating factors
library(flextable)
library(tidytable)
library(glue) #for pasting strings
library(shinymanager)
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app


############################################### Data ###############################################

tidyLFT <-readRDS("data/tidyLFT.rds")
CHICapture <-readRDS("data/CHICapture.rds")
TestNumbers <- readRDS("data/test_numbers.rds")
TestNumbersChart <- readRDS("data/test_numbers_chart.rds")
TestNumbersChartRoll <- readRDS("data/roll_test_numbers_chart.rds")
LFT_hb <- readRDS("data/LFT_hb.rds")
LFT_sc <- readRDS("data/LFT_sc.rds")

###############################################Functions###############################################

plot_box <- function(title_plot, plot_output) {
  tagList(h4(title_plot),
          withSpinner(plotlyOutput(plot_output)))
}

# plot_cut_box <- function(title_plot1, plot_output1,
#                          title_plot2, plot_output2, extra_content = NULL) {
#   tagList(
#     fluidRow(column(6, h4(title_plot1)),
#              column(6, h4(title_plot2))),
#     extra_content,
#     fluidRow(column(6, withSpinner(plotlyOutput(plot_output1))),
#              column(6, withSpinner(plotlyOutput(plot_output2))))
#   )
# }

#if missing plot (e.g. no SIMD)
plot_cut_missing <- function(title_plot, plot_output, extra_content = NULL) {
  tagList(
    fluidRow(column(6, h4(title_plot))),
    extra_content,
    fluidRow(column(6, withSpinner(plotlyOutput(plot_output))))
  )
}


############################################### Data lists ###############################################
#Create a function which creates all unique  
Profession <- c(sort(unique(tidyLFT$test_cohort_name)))
Work_Location <- c(unique(tidyLFT$LocationName))
LFT_Profession <- c(sort(unique(LFT_hb$test_cohort_name)))
Time_period <- c(sort(unique(LFT_hb$time_period)))
LFT_NHS_Board <- c(sort(unique(LFT_hb$Health_Board_Name)))


data_list_data_tab <- c("Daily number of tests by result" = "tidyLFT",
                        "Number of tests" = "TestNumbers")

CHIText <- paste0("Data is shown only for cases where a valid Unique Patient Identifier was submitted. Data completeness is ", 
                  CHICapture$CHICapture, "% (The total number of cases is ", CHICapture$Total, " of which ", CHICapture$Count," were submitted with a valid UPI)" )

############################################### Palettes ###############################################

# pal_overall <- c('#9B4393', '#0078D4','#000000', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')

pal_tests <- c("NEGATIVE" = "#0078D4", "INCONCLUSIVE" = "#9B4393", 
               "POSITIVE" = "#000000")

pal_n_tests <- c("1" = "#082359", "2" = "#0078D4", "3" = "#9B4393", 
                 "4" = "#dcb4e9", "5" = "#e9b4e0", "6" = "#00cdd4",
                 "7" = "#7fcdbb", "8" = "#7fcd99", "9" = "#94cd7f", 
                 "10+" = "#000000")


############################################### Plot Parameters ###############################################

# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

############################################### END ###############################################
