# Global


############################################### Dates ###############################################

#publication date
pub_date <- as.Date("2021-01-27")

# Data extraction date
admission_extract_date <- format(pub_date - 3, "%A %d %B %Y") #format date


############################################### Packages ###############################################
library(devtools)
devtools::install_github("andrewsali/shinycssloaders")
library(shiny)
library(plotly) # for charts
library(shinyWidgets) # for dropdowns
library(dplyr) # for data manipulation
library(DT) # for data table
library(shinyjs) # for enable/disable functions
library(readr) # for writing/reading csvs
library(stringr) #for manipulating strings
library(forcats) #manipulating factors
library(flextable)
library(tidytable)
library(shinyBS) #for collapsible panels in commentary
library(glue) #for pasting strings
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app

############################################### Functions ###############################################

plot_box <- function(title_plot, plot_output) {
  tagList(h4(title_plot),
          withSpinner(plotlyOutput(plot_output)))
}

plot_cut_box <- function(title_plot1, plot_output1,
                         title_plot2, plot_output2, extra_content = NULL) {
  tagList(
    fluidRow(column(6, h4(title_plot1)),
             column(6, h4(title_plot2))),
    extra_content,
    fluidRow(column(6, withSpinner(plotlyOutput(plot_output1))),
             column(6, withSpinner(plotlyOutput(plot_output2))))
  )
}

#if missing plot (e.g. no SIMD)
plot_cut_missing <- function(title_plot, plot_output, extra_content = NULL) {
  tagList(
    fluidRow(column(6, h4(title_plot))),
    extra_content,
    fluidRow(column(6, withSpinner(plotlyOutput(plot_output))))
  )
}


############################################### Data ###############################################

LFTextract <-readRDS("data/LFTextract.rds")
LabCases <-readRDS("data/LabCases.rds")


############################################### Data lists ###############################################


data_list <- c("Positive Cases" = "LabCases")

#LFTWorkLocation <- c(sort(unique(Settings$`Setting Type`)))

# choices for data tables
data_list_data_tab <- c("Positive Cases" = "LabCases")


############################################### Palettes ###############################################

pal_overall <- c('#000000', '#0078D4','#9B4393', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')


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
