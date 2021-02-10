secure_app( #uncomment if needing password protection
#UI
tagList(  #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(
    id = "intabset",# id used for jumping between tabs
    title = div(
      tags$a(img(src = "phs-logo.png", height = 40), href = "https://www.publichealthscotland.scot/"),
      style = "position: relative; top: -5px;"),
    windowTitle = "PHS COVID-19 Lateral Flow Tests",    #title for browser tab
    header = tags$head(includeCSS("www/styles.css"),  # CSS styles
                       tags$link(rel = "shortcut icon", href = "favicon_phs.ico"),
                       includeHTML(("www/google-analytics.html"))), #Icon for browser tab     
    # includeScript("data/google-analytics.js")), #Including Google analytics
    
############################################### Introduction ###############################################
    tabPanel("Introduction",
             icon = icon("info-circle"),
             value = "intro",
             h3("COVID-19 Lateral Flow Tests"),
             h3("Background"),
             p("Text goes here"),
             p(""),
             
             h3("Further information"),
             
             p("There is a large amount of data being regularly published regarding COVID-19. 
               This dashboard complements the range of existing data currently available."),
             
             p("Information on surveillance and monitoring of COVID-19 amongst the population are available in the ",
               tags$a(
                 href = "https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/",
                 "COVID-19 weekly report for Scotland.",
                 class = "externallink")),
             
             p("Information on the wider impacts on the health care system from COVID-19 are available on the ",
               tags$a(
                 href = "https://scotland.shinyapps.io/phs-covid-wider-impact/",
                 "Wider Impacts dashboard.",
                 class = "externallink")),
             
             p("Information and support on a range of topics in regards to COVID-19 are available on the ",
               tags$a(
                 href = "https://www.gov.scot/coronavirus-covid-19/",
                 "Scottish Government website.",
                 class = "externallink")),
             
             p("Information on deaths involving COVID-19 is available on the ",
               tags$a(
                 href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-and-monthly-data-on-births-and-deaths/deaths-involving-coronavirus-covid-19-in-scotland/",
                 "National Records Scotland website.",
                 class = "externallink")),
             
             h3("Contact"),         
             p("If you have any questions relating to these data please contact: ",
               tags$b(
                 tags$a(
                   href = "mailto:phs.covidweeklyreport@phs.scot", # needs updated
                   "phs.covidweeklyreport@phs.scot", # needs updated
                   class = "externallink")),"."),
             p("If you have a media query please contact: ",
               tags$b(
                 tags$a(
                   href = "mailto:phs.comms@nhs.net",
                   "phs.comms@nhs.net",
                   class = "externallink")),".")#,
                       ), #tabPanel bracket
    
    
############################################### Trend Charts ###############################################
    
    tabPanel(
      title = "Trend Charts",
      icon = icon("area-chart"),
      value = "summary",
      wellPanel(
        column(4,
               div(title = "Select profession.", # tooltip
                   selectInput("Profession_select", label = h3("Select the data you want to explore."), 
                               choices = Profession))),

        column(4,
               div(title = "Select work location", # tooltip
                   pickerInput("Location_select", label = h3("Select the data you want to explore."),
                               choices = NULL,
                               selected = NULL,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE))),

        column(4,
               
               actionButton("btn_dataset_modal", paste0("Data source: ", "ECOSS"), icon = icon('question-circle')))
        
        ), #wellPanel bracket
      
      mainPanel(width = 12,
                uiOutput("data_explorer")
                
      )# mainPanel bracket
      
    ),# tabpanel bracket
    
############################################### Data ###############################################
    tabPanel(
      title = "Data",
      icon = icon("table"),
      value = "table",
     
      p("This section allows you to view the data in table format.
        You can use the filters to select the data you are interested in.
        You can also download the data as a csv using the download button."),
      
      p(paste0(CHIText)), 
      
      column(6,        
             selectInput("data_select", "Select the data you want to explore.",
                         choices = data_list_data_tab)),  
      
      column(6, downloadButton('download_table_csv', 'Download data')),
      
      mainPanel(width = 12,
                DT::dataTableOutput("table_filtered"))
      
    )# tabpanel bracket
    
    
############################################### End ###############################################
    
      ) # navbarPage bracket
             ) # taglist bracket
) # secure app

##END