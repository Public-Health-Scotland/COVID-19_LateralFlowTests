# Global
###############################################.

## Dates ----

#publication date
pub_date <- as.Date("2021-01-27")

# Data extraction date
admission_extract_date <- format(pub_date - 3, "%A %d %B %Y") #format date

###############################################.
## Packages ----

library(shiny)
library(plotly) # for charts
library(shinyWidgets) # for dropdowns
library(dplyr) # for data manipulation
library(DT) # for data table
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(shinyjs) # for enable/disable functions
library(readr) # for writing/reading csvs
library(stringr) #for manipulating strings
library(forcats) #manipulating factors
library(flextable)
library(tidytable)
library(shinyBS) #for collapsible panels in commentary
library(glue) #for pasting strings

###############################################.
## Functions ----

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

###############################################.
## Data ----

LFTextract <-readRDS("data/LFTextract.rds")
LabCases <-readRDS("data/LabCases.rds")
# Admissions <-readRDS("data/Admissions.rds")
# ICU <- readRDS("data/ICU.rds")
# NHS24 <- readRDS("data/NHS24.rds")
# AssessmentHub <- readRDS("data/AssessmentHub.rds")
# SAS <- readRDS("data/SAS.rds")
# NHS24_inform <- readRDS("data/NHSInform.rds")
# NHS24_selfhelp <- readRDS("data/SelfHelp.rds")

# #read age/sex/deprivation data
# LabCases_AgeSex <- readRDS("data/LabCases_AgeSex.rds")
# LabCases_SIMD <- readRDS("data/LabCases_SIMD.rds")
# Admissions_AgeSex <- readRDS("data/Admissions_AgeSex.rds")
# Admissions_SIMD <- readRDS("data/Admissions_SIMD.rds")
# ICU_AgeSex <- readRDS("data/ICU_AgeSex.rds")
# NHS24_AgeSex <- readRDS("data/NHS24_AgeSex.rds")
# NHS24_SIMD <- readRDS("data/NHS24_SIMD.rds")
# AssessmentHub_AgeSex <- readRDS("data/AssessmentHub_AgeSex.rds")
# AssessmentHub_SIMD <- readRDS("data/AssessmentHub_SIMD.rds")
# SAS_AgeSex <- readRDS("data/SAS_AgeSex.rds")
# SAS_SIMD <- readRDS("data/SAS_SIMD.rds")
# 
# #read SAS/NHS24 other data
# NHS24_community <- readRDS("data/NHS24_community.rds")
# SAS_all <- readRDS("data/SAS_all.rds")
# 
# # read in children data
# ChildCases <- readRDS("data/ChildCases.rds")
# ChildTests <- readRDS("data/ChildTests.rds")
# Child <- readRDS("data/Child.rds")
# 
# 
# # Ethnicity
# Ethnicity <- readRDS("data/Ethnicity.rds")
# Ethnicity_Chart <- readRDS("data/Ethnicity_Chart.rds")

###############################################.
## Data lists -------------------------------------------------------------- 


data_list <- c("Positive Cases" = "LabCases")

#LFTWorkLocation <- c(sort(unique(Settings$`Setting Type`)))

# choices for data tables
data_list_data_tab <- c("Positive Cases" = "LabCases")

###############################################.
## Palettes  ----

pal_overall <- c('#000000', '#0078D4','#9B4393', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')

###############################################.
## Plot Parameters ---------------------------------------------------------

# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

## END
