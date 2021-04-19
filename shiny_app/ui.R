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
             p("This is a work in progress dashboard on management information 
               for Lateral Flow results. As of 16th April 2021 this dashboard 
               now contains information on lateral flow tests via the NSS Portal 
               tool or UK Government."),
            
             p("Data for Lateral Flow tests can contain self-service 
               information, thus the data quality is variable."),
             
             p("Please note that CHI seeding for negative results has been 
               turned back on since 9th March 2021."),
             
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
    
    ############################################### Daily LFT Report ###############################################
    
    # tabPanel(
    #   title = "Daily LFT Report",
    #   icon = icon("table"),
    #   
    #   p("Please note the data in this tab uses NHS Board of residence."), 
    #   
    #   h3("Section A - UK Govt"),
    #   
    #   
    #   fluidRow(
    #     h4("Table 1: Daily new positive and negative LFD Tests"),
    #     
    #     mainPanel(width = 7,
    #               DT::dataTableOutput("LFT_table_1")
    #               
    #     )), 
    #     
    #     column(4, downloadButton('LFT_download_table_1_csv', 'Download data')), 
    #   
    #   br(),
    #   br(), 
    #   br(), 
    #   
    #   fluidRow(
    #   h4("Table 2: Total number of LFD Tested Individuals*"),
    #   
    #   mainPanel(width = 7,
    #             DT::dataTableOutput("LFT_table_2")
    #             
    #   )), 
    #   
    #   p("*The following figures relate to individuals who have a valid CHI for 
    #     all test results. Tests without valid CHI are excluded."), 
    #   
    #   column(4, downloadButton('LFT_download_table_2_csv', 'Download data')), 
    #   
    #   br(),
    #   br(),
    #   br(),
    #   
    #   fluidRow(
    #     h4("Table 3: Cumulative Number of LFD Tests by NHS Board"),
    #     
    #     mainPanel(width = 9,
    #             DT::dataTableOutput("LFT_table_3")
    #             
    #   )), 
    #   
    #   column(4, downloadButton('LFT_download_table_3_csv', 'Download data')), 
    #   
    #   br(),
    #   br(),
    #   br(),
    #   
    #   # mainPanel(width = 9,
    #   #           DT::dataTableOutput("LFT_table_4")
    #   #           
    #   # ), 
    #   # 
    #   # column(4, downloadButton('LFT_download_table_4_csv', 'Download data')), 
    #   
    #   h3("Section B - NSS Portal"),
    #   
    #   fluidRow(
    #     h4("Table 5: Daily new positive and negative LFD Tests via NSS Portal"),
    #     
    #     mainPanel(width = 9,
    #             DT::dataTableOutput("LFT_table_5")
    #             
    #   )), 
    #   
    #   p("The NSS Portal data is dynamic and subject to change with each new LFD 
    #     programme roll-out. Test results are 
    #     submitted via the NSS Portal app where users manually input their test 
    #     results."), 
    #   
    #   
    #   column(4, downloadButton('LFT_download_table_5_csv', 'Download data')), 
    #   
    #   br(),
    #   br(),
    #   br(),
    #   
    #   fluidRow(
    #     h4("Table 6a: Cumulative number of LFD Tests by Category carried out via NSS Portal"),
    #     
    #     mainPanel(width = 9,
    #             DT::dataTableOutput("LFT_table_6a")
    #             
    #   )), 
    #   
    #   column(4, downloadButton('LFT_download_table_6a_csv', 'Download data')), 
    #   
    #   br(),
    #   br(),
    #   br(),
    #   
    #   fluidRow(
    #     h4("Table 6b: Cumulative number of LFD Tests and Individuals by Category carried out via NSS Portal"),
    #     
    #     mainPanel(width = 9,
    #             DT::dataTableOutput("LFT_table_6b")
    #             
    #   )), 
    #   
    #   p("*The following figures relate to individuals who have a valid CHI for 
    #     all test results. Tests without valid CHI are excluded."), 
    #   
    #   column(4, downloadButton('LFT_download_table_6b_csv', 'Download data')), 
    #   
    #   br(),
    #   br(),
    #   br(),
    #   
    #   fluidRow(
    #     h4("Table 7: Cumulative number of LFD Tests and Individuals by Health Board carried out via NSS Portal"),
    #     
    #     mainPanel(width = 9,
    #             DT::dataTableOutput("LFT_table_7")
    #             
    #   )), 
    #   
    #   p("*The following figures relate to individuals who have a valid CHI for 
    #     all test results. Tests without valid CHI are excluded."), 
    #   
    #   column(4, downloadButton('LFT_download_table_7_csv', 'Download data')) # tabpanel bracket
    #   
    #   ), 
  
    
    ############################################### LFT Tests ###############################################
    
    tabPanel(
      title = "NHS Board Data",
      icon = icon("table"),
      
      p("This section allows you to view the total number of LFTs and the 
        number of positive LFTs within Scotland and each NHS Board for each
        profession. You can use the filters to select the data you are interested in.
        You can also download the data within each table as a csv using the 
        download button underneath each table."),
      
      p("The data in this tab uses NHS Board of employment to allocate board
        level data."),
      
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
      
      column(4, downloadButton('LFT_download_table_csv_scot', 'Download data')), 
      
      br(), 
      br(), 
      br(), 
      
      
      mainPanel(width = 12,
                DT::dataTableOutput("LFT_hb_table_filtered")# mainPanel bracket
                
      ), 
      
      column(4, downloadButton('LFT_download_table_csv', 'Download data')), 
      
      br()# tabpanel bracket
      
    ), 
    
    
    
    ############################################### Trend Charts ###############################################
    
    tabPanel(
      title = "Trend Charts",
      icon = icon("area-chart"),
      
      p("The data in this tab uses NHS Board of employment to allocate board
        level data."),
      
      p(paste0(summary_text_1)), 
      
      p(paste0(summary_text_2)), 
      
      p(paste0(summary_text_3)), 
      
      p(paste0(summary_text_4)), 
      
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
    
    ############################################### LFT to PCR ###############################################
    
    tabPanel(
      title = "LFT to PCR",
      icon = icon("table"),

      p("This section shows individuals who have had a positive LFT
         who go on to have a PCR test within the following 48 hours. The table
        below shows the number of individuals with positive LFTs and their
        subsequent PCR result split by their test group and NHS Board of 
        employment. You can use the dropdowns to select what test group or 
        NHS Board you are interested in."), 
      
      p("Individuals have been linked using CHI to identify PCR results 
        (via NHS or UK Gov Labs) within 48 hours of tests being entered into 
        LFT Portal. Only one positive LFT a day per person is counted. Only the 
        first PCR within 48 hours of the positive LFT is counted. Percentages
        are calculated using the number of positive or negative PCR tests
        divided by the total number of positive PCRs within 48 hours of a 
        positive LFT"), 
      
      p("We cannot confirm that the PCR within 48 hours is a confirmatory PCR 
        as some individuals are taking a combination of LFTs/PCRs on a regular 
        basis"), 
      
      p(paste0(lft_pcr_text_1)), 

      wellPanel(
        column(4,
             div(title = "Select profession.", # tooltip
                 selectInput("LFT_PCR_profession_select", label = h3("Select the data you want to explore."), 
                             choices = lft_pcr_group))),
      
      column(4,
             div(title = "Select work location", # tooltip
                 pickerInput("LFT_PCR_hb_select", label = h3("Select the data you want to explore."),
                             choices = NULL,
                             selected = NULL,
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE)))#,
      ), #wellPanel bracket
      
      mainPanel(width = 9,
                DT::dataTableOutput("LFT_PCR_table_filtered")
                
      ), 
      
      column(4, downloadButton('LFT_PCR_download_table_csv', 'Download data'))
      
    ), 
    
    ############################################### Data ###############################################
    tabPanel(
      title = "Data",
      icon = icon("table"),
      value = "table",
      
      p("This section allows you to view the data in table format.
        You can use the filters to select the data you are interested in.
        You can also download the data as a csv using the download button."),
      
      p("The data in this tab uses NHS Board of employment to allocate board
        level data."),
      
      p(paste0(CHIText)), 
      
      p(paste0(data_tab_text_1)), 
      
      column(6,        
             selectInput("data_select", label = h3("Select the data you want to explore."),
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