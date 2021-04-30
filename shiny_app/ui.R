secure_app( #uncomment if needing password protection
#UI
tagList(  #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(
    id = "intabset",# id used for jumping between tabs
    title = div(
      tags$a(img(src = "phs-logo.png", height = 40), 
             href = "https://www.publichealthscotland.scot/",  target = "blank_"),
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
    
    tabPanel(
      title = "Daily LFT Report",
      icon = icon("book-medical"),
      h3("Daily LFT Report"),
      p(paste0("This section allows you to view the dialy LFD data, and generate a report.",
               "You can choose the report date and which outputs to include using the filters below.",
               "Please note the data in this tab use NHS Board of residence.")),
      hr(),
      fluidRow(
        column(width = 3, 
               shiny::dateInput("lft_date_select", label = "Report Date", value = lft_date_range$max,
                                min = lft_date_range$min, max = lft_date_range$max)
        ),
        column(width = 5, 
               pickerInput("lft_table_select", label = "Outputs", 
                           choices = lft_table_list, multiple = TRUE, 
                           selected = lft_table_list,width = "100%")
        ),
        column(width = 2,offset = 2, 
               downloadButton("lft_report_render_dwnl", 
                              label = "Generate Report",
                              class = "btn btn-primary",style = "float:right;margin-bottom:10px"),
               downloadButton("lft_excel_dwnl", 
                              label = "Download Excel Tables",
                              class = "btn btn-success",style = "float:right;"))),
      hr(),
      uiOutput("daily_lft_tab"),
      br3(), br3()
    ),
    
    
    ############################################### LFT Tests ###############################################
    
    tabPanel(
      title = "NHS Board Data",
      icon = icon("table"),
      
      h3("NHS Health Board Data"),
      p("This section allows you to view the total number of LFTs and the 
        number of positive LFTs within Scotland and each NHS Board for each
        profession. You can use the filters to select the data you are interested in.
        You can also download the data within each table as a csv using the 
        download button underneath each table."),
      
      p("The data in this tab uses NHS Board of employment to allocate board
        level data."),
      
      p(paste0(hb_text_1)), 
      
      p(paste0(hb_text_2)), 
      
      hr(),
      fluidRow(
        column(width = 4,
               div(title = "Select profession", # tooltip
                   selectInput("LFT_profession_select", label = "Profession",
                               choices = LFT_Profession))),
        column(width = 5,
               div(title = "time_period_select", # tooltip
                   selectInput("Time_select", label = "Time Period",
                               choices = Time_period))),
        
        column(width = 2, offset = 1, br(), 
               actionButton("btn_dataset_modal", paste0("Data source: ", "ECOSS"), 
                            icon = icon('question-circle'),
                            class = "btn btn-warning", style = "float:right;"))
      ), #fluidRow bracket 
      
      hr(),
      dataTableOutput("LFT_sc_table_filtered"),
      br3(),
      dataTableOutput("LFT_hb_table_filtered"),
      br3(), br3(), br3()
      
    ), #tabPanel bracket
    
    tabPanel(
      title = "Primary Care and Healthcare Workers",
      icon = icon("user-md"),
      
      h3("Primary Care and Healthcare Worker Data"),
      p("This section allows you to view the cumulative number of LFTs taken by
        Primary Care Workers and Healthcare Workers by their submitted Job Role."),
      strong("Caveats:"),
      tags$i(
        tags$ol(
          tags$li("The data in this tab use NHS Board of employment to allocate board
                  level data."),
          tags$li("These data are based on the Job Role submitted by the individual.
                  Please note that this field is incomplete, and completeness levels vary
                  between professions.", br(), "The percent of job roles recorded is included
                  for both Primary Care Workers and Healthcare Workers as an indication of this.")
        )), # italics bracket  (caveats)
      
      hr(),
      fluidRow(
        column(width = 4,
               prettyRadioButtons("pc_hcw_radio_select", label = "Profession", 
                                  choices = c("Primary Care Workers", "Healthcare Workers"),
                                  inline = TRUE, shape = "curve", icon = icon ("check"),
                                  thick = FALSE, animation = "pulse")),
        uiOutput("pc_hwc_tab_widget_ui")
      ),
      hr(),
      uiOutput("pc_hcw_tab_main_ui"),
      br3(), br3(), br3()
    ), # tabPanel bracket
    
    
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
      
      fluidRow(
        column(4,
               div(title = "Select profession.", # tooltip
                   selectInput("LFT_PCR_profession_select", label = "Profession", 
                               choices = lft_pcr_group))),
        
        column(4,
               div(title = "Select work location", # tooltip
                   pickerInput("LFT_PCR_hb_select", label = "Health Board",
                               choices = NULL,
                               selected = NULL,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE)))#,
      ), #fluidRow bracket
      
      dataTableOutput("LFT_PCR_table_filtered"),
      
      br3(), br3(), br3()
      
    ), # tabPanel bracket
    
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
             selectInput("data_select", label = "Select the data you want to explore",
                         choices = data_list_data_tab)),  
      
      mainPanel(width = 12,
                DT::dataTableOutput("table_filtered"),
      
      downloadButton('download_table_csv', 
                               'Download data',
                               class = "btn btn-primary",
                               style = "float:right;margin-bottom:10px"),
      br3(),br3(),br3()
      
      ) #mainPanel
      
      )# tabpanel bracket
    
    
    ############################################### End ###############################################
    
    ) # navbarPage bracket
) # taglist bracket
) # secure app

##END