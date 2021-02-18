#Server side
credentials <- readRDS("admin/credentials.rds")

function(input, output, session) {
  
  ##################### Shinymanager Auth ########################## 
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  

  ###############################################.
  # Sourcing file with functions code
  source(file.path("functions_server.R"),  local = TRUE)$value
  
  ###############################################.
  # NHS Boards data tab  
  source(file.path("hb_tab.R"),  local = TRUE)$value
  
  ###############################################.
  # Summary trends tab  
  source(file.path("summary_tab.R"),  local = TRUE)$value
  
  ###############################################.
  # Data tab
  source(file.path("data_tab.R"),  local = TRUE)$value
  
  ## Observe events to improve navigation between tabs of the app
  # To jump to data pages    
  observeEvent(input$jump_to_summary, {updateTabsetPanel(session, "intabset", selected = "summary")})
  observeEvent(input$jump_to_table, {updateTabsetPanel(session, "intabset", selected = "table")})

} # server end