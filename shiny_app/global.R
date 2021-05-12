# Global

############################################### Dates ###############################################

#publication date
pub_date <- as.Date("2021-04-30")


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
library(lubridate)
library(rmarkdown)
library(knitr)
library(writexl)
library(janitor)
library(magrittr)
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app


############################################### Data ###############################################

weekly_chart_complete <- readRDS("data/weekly_chart_complete.rds")
cumulative_tests <- readRDS("data/cumulative_tests.rds")
tidyLFT <- readRDS("data/tidyLFT.rds")
CHICapture <- readRDS("data/CHICapture.rds")
PosRate <- readRDS("data/positivity_rate.rds")
TestNumbers <- readRDS("data/test_numbers.rds")
TestNumbersRoll <- readRDS("data/roll_test_numbers.rds")
TestNumbersChart <- readRDS("data/test_numbers_chart.rds")
TestNumbersChartRoll <- readRDS("data/roll_test_numbers_chart.rds")
LFT_hb <- readRDS("data/LFT_hb.rds")
LFT_sc <- readRDS("data/LFT_sc.rds")
dates <- readRDS("data/dates.rds")
lft_pcr <- readRDS("data/lft_pcr_hb.rds")

pc_test_reasons <- readRDS("data/PrimaryCare_TestReasons_sc.rds")
hcw_test_reasons <- readRDS("data/HCW_TestReasons_hb.rds")
hcw_test_reasons_hosp <- readRDS("data/HCW_TestReasons_hospital.rds")
hcw_hosp_weekly <- readRDS("data/hcw_job_role_hosp_by_week.rds")

table_1 <- readRDS("data/table_1.rds")
table_2 <- readRDS("data/table_2.rds")
table_3 <- readRDS("data/table_3.rds")
table_5 <- readRDS("data/table_5.rds")
table_6a <- readRDS("data/table_6a.rds")
table_6b <- readRDS("data/table_6b.rds")
table_7 <- readRDS("data/table_7.rds")

uk_gov_av <- table_1 %>% 
  filter(COVID.19 == "Total number of newly reported LFD Tests in NHS and UK Govt testing sites")%>%
  mutate(Daily = as.numeric(gsub(",","",Daily))) %>%
  summarise(n_av = format(round_half_up(mean(Daily),0), big.mark = ","), 
            n_max = format(max(Daily),big.mark = ","))

nss_portal_av <- table_5 %>% 
  filter(COVID.19 == "Total number of newly reported LFD Tests via NSS Portal")%>%
  mutate(Daily = as.numeric(gsub(",","",Daily))) %>%
  summarise(n_av = format(round_half_up(mean(Daily),0), big.mark = ","), 
            n_max = format(max(Daily),big.mark = ","))



###############################################Functions###############################################

plot_box <- function(title_plot, subtitle_plot = "", plot_output) {
  tagList(h4(title_plot),p(tags$i(subtitle_plot)),
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

br3<-function(){tagList(br(),br(),br())}


############################################### Data lists ###############################################
#Create a function which creates all unique  
Profession <- c(sort(unique(tidyLFT$test_cohort_name)))
Work_Location <- c(unique(tidyLFT$LocationName))
LFT_Profession <- c(sort(unique(LFT_hb$test_cohort_name)))
Time_period <- c(sort(unique(LFT_hb$time_period)))
LFT_NHS_Board <- c(sort(unique(LFT_hb$Health_Board_Name)))
lft_pcr_hb <- c(sort(unique(lft_pcr$`NHS Board of Employment`)))
lft_pcr_group <- c(sort(unique(lft_pcr$`Test Group`)))

test_source <- c("UK Gov", "NSS")

cumulative_dates <- dates %>% filter(data == "cumulative")
last_7_dates <- dates %>% filter(data == "last_7_days")
data_dates <- dates %>% filter(data == "test_numbers")
chart_dates <- dates %>% filter(data == "test_numbers_chart")
roll_chart_dates <- dates %>% filter(data == "roll_test_numbers_chart")


data_list_data_tab <- c("Daily number of tests by result" = "tidyLFT",
                        "Number of tests by individual" = "TestNumbers", 
                        "Four week rolling number of tests by individual" = "TestNumbersRoll")

CHIText <- paste0("Data for the number of tests per individual is shown only for 
                  cases where a valid Unique Patient Identifier was submitted. Data completeness is ", 
                  CHICapture$CHICapture, "% (The total number of cases is ", CHICapture$Total, " of which ", CHICapture$Count," were submitted with a valid UPI)" )

hb_text_1 <- paste0("Cumulative data covers tests from ", cumulative_dates$min_date, " to ",  cumulative_dates$max_date, ".")

hb_text_2 <- paste0("Last 7 days covers tests from ", last_7_dates$min_date, " to ",  last_7_dates$max_date, ".")

summary_text_1 <- paste0("Data for the weekly number of tests by result and 
                         weekly positivity rate covers tests from ",
                         cumulative_dates$min_date, " to ",  chart_dates$max_date, ".")

summary_text_2 <- paste0("Data for the number of tests by NHS Board covers tests from ",
                         cumulative_dates$min_date, " to ",  cumulative_dates$max_date, ".")

summary_text_3 <- paste0("Data for number of tests per individual by work location in latest week
                         covers tests from ", chart_dates$min_date, " to ",
                         chart_dates$max_date, ".")

summary_text_4 <- paste0("Data for number of tests per
                         individual by work location in latest rolling four week period
                         covers tests from ", roll_chart_dates$min_date, " to ",
                         roll_chart_dates$max_date, ".")

lft_pcr_text_1 <- paste0("Data in the table below covers tests from ", 
                          cumulative_dates$min_date, " to ",  cumulative_dates$max_date, ".")

data_tab_text_1 <- paste0("Data in the table below covers tests from ", 
                          cumulative_dates$min_date, " to ",  cumulative_dates$max_date, ".")

data_tab_text_2 <- paste0("Data on the number of tests per individual covers tests from
                          ", data_dates$min_date, " to ",  data_dates$max_date, ".")

############################################### LFT Report Tab #########################################
lft_date_range <- list(min = min(as_date(table_1$day)),
                       max = max(as_date(table_1$day))) 

lft_table_list <- c("Daily new positive and negative LFD Tests",
                    "Total number of LFD Tested Individuals",
                    "Cumulative Number of LFD Tests by NHS Board",
                    "Daily new positive and negative LFD Tests via NSS Portal",
                    "Cumulative number of LFD Tests by Category carried out via NSS Portal",
                    "Cumulative number of LFD Tests and Individuals by Category carried out via NSS Portal",
                    "Cumulative number of LFD Tests and Individuals by Health Board carried out via NSS Portal")



############################################### Palettes ###############################################

pal_overall <- c('#9B4393', '#0078D4','#000000', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')

pal_tests <- c("Negative" = "#0078D4", "Inconclusive" = "#9B4393", 
               "Positive" = "#000000", "Insufficient" = "#9b4347")

pal_n_tests <- c("1" = "#082359", "2" = "#0078D4", "3" = "#9B4393", 
                 "4" = "#dcb4e9", "5" = "#e9b4e0", "6" = "#00cdd4",
                 "7" = "#7fcdbb", "8" = "#7fcd99", "9" = "#94cd7f", 
                 "10+" = "#000000")

pal_pos <- c("Ayrshire and Arran" = "#082359", "Borders" = "#0078D4", 
             "Dumfries and Galloway" = "#9B4393", "Fife" = "#dcb4e9", 
             "Forth Valley" = "#e9b4e0", "Grampian" = "#00cdd4",
             "Greater Glasgow and Clyde" = "#7fcdbb", "Highland" = "#7fcd99", 
             "Lanarkshire" = "#94cd7f", "Lothian" = "#cd9d7f", 
             "Orkney" = "#bfcd7f", "Shetland" = "#cdbf7f", 
             "Tayside" = "#cdaa7f", "Unknown" = "#000000", 
             "Western Isles" = "#cd7f7f", 
             "Healthcare Improvement Scotland" = "#7fcd80", 
             "NHS Education for Scotland" = "#7fcdb0", 
             "NHS Quality Improvement Scotland" = "#7fcdb0", 
             "NHS National Services Scotland" = "#7fb7cd", 
             "National Facility" = "#7f98cd", 
             "NHS24" = "#917fcd", "Public Health Scotland" = "#ae7fcd", 
             "Scotland" = "932fcd", 
             "Scottish Ambulance Service" = "#cd7fc5", 
             "State Hospital" = "#cd7f93", 
             "Care Inspectorate " = "#7fcd9a")


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
