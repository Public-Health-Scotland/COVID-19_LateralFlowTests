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
             p("This is a work in progress dashboard on management information for Lateral 
               Flow results coming from the NSS Portal only. Development work is 
               ongoing to introduce all LFD results.  Information on PCR 
               follow-up will be added by 1st March 2021."),
            
             p("The NSS Portal tool is a self-service tool thus the data quality is variable."),
             
             # p(strong("Please note we have been made aware that there is a backlog of data 
             #   for 25th and 26th February 2021. This data should be treated with caution 
             #   and will be updated next week.")),
             
             h3("Further information"),
             
             p("A registration tool is being developed to record more consistent information."), 
             
             p("Carehomes have been mapped to NHS Board."), 
             
             p("Council areas have been mapped to NHS Boards."), 
             
             p("The NSS tool can be accessed: http://covidtestingportal.scot/"), 
             
             h3("Contact"),         
             p("If you have any questions relating to these data please contact: ",
               tags$b(
                 tags$a(
                   href = "mailto:PHS.Covid19Data&Analytics@phs.scot", # needs updated
                   "PHS.Covid19Data&Analytics@phs.scot", # needs updated
                   class = "externallink")),".")
             ), #tabPanel bracket
    
    ############################################### LFT Tests ###############################################
    
    tabPanel(
      title = "NHS Board Data",
      icon = icon("table"),
      
      p("This section allows you to view the total number of LFTs and the 
        number of positive LFTs within Scotland and each NHS Board for each
        profession. You can use the filters to select the data you are interested in.
        You can also download the NHS board data as a csv using the download button."),
      
      p(paste0(hb_text_1)), 
      
      p(paste0(hb_text_2)), 
      
      wellPanel(
        column(4,
               div(title = "Select profession", # tooltip
                   selectInput("LFT_profession_select", label = h3("Select the data you want to explore."),
                               choices = LFT_Profession))),
        column(5,
               div(title = "time_period_select", # tooltip
                   selectInput("Time_select", label = h3("Select the time period you want to explore."),
                               choices = Time_period)))#,
        
        # column(4,
        #        
        #        actionButton("btn_dataset_modal", paste0("Data source: ", "ECOSS"), icon = icon('question-circle'))), 
        
        
      ), #wellPanel bracket

      mainPanel(width = 9,
                DT::dataTableOutput("LFT_sc_table_filtered")

      ), 
      
      column(4, downloadButton('LFT_download_table_csv', 'Download data')), 
      
      
      mainPanel(width = 12,
                DT::dataTableOutput("LFT_hb_table_filtered")# mainPanel bracket
                
      )# tabpanel bracket
      
    ), 
    
    
    
    ############################################### Trend Charts ###############################################
    
    tabPanel(
      title = "Trend Charts",
      icon = icon("area-chart"),
      value = "summary",
      
      p(paste0(summary_text_1)), 
      
      p(paste0(summary_text_2)), 
      
      p(paste0(summary_text_3)), 
      
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
                               multiple = TRUE)))#,
        
        # column(4,
        #        
        #        actionButton("btn_dataset_modal", paste0("Data source: ", "ECOSS"), icon = icon('question-circle')))
        # 
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
      
      p(paste0(data_tab_text_1)), 
      
      p(paste0(data_tab_text_2)), 
      
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