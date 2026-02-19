# Post-Con Status App v 1.0.0
# Author: Farshad Ebrahimi
# Last changed: 10/05/2023

# SET UP
#0.0: load libraries --------------
  #shiny
  library(shiny)
  #pool for database connections
  library(pool)
  #odbc for database connections
  library(odbc)
  #tidyverse for data manipulations
  library(tidyverse)
  #shinythemes for colors
  library(shinythemes)
  #lubridate to work with dates
  library(lubridate)
  #shinyjs() to use easy java script functions
  library(shinyjs)
  #DT for datatables
  library(DT)
  #reactable themes
  library(reactablefmtr)
  #reactable for reactable tables
  library(reactable)
  #excel download
  library(xlsx)
  library(DBI)
  #Rpostgres for dbcon
  library(RPostgres)
  #Package Versioning
  library(renv)
  #css
  library(shinycssloaders)
#Not in logical
  `%!in%` <- Negate(`%in%`)

#0.1: database connection and global options --------

#set default page length for datatables
  options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
  #poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
  poolConn <- dbPool(RPostgres::Postgres(),
                 dbname = 'mars_prod', 
                 host = 'PWDMARSDBS1', 
                 port = 5434, 
                 user = Sys.getenv("shiny_uid"),
                 password = Sys.getenv("shiny_pwd"))

#disconnect from db on stop 
  onStop(function(){
    poolClose(poolConn)
  })

#js warning about leaving page
  jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'
  
  # fiscal quarter lookup
  fq_lookup <- dbGetQuery(poolConn,"select * from admin.tbl_fiscal_quarter_lookup")
  
  # post-con status look up
  status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup")
  
#status list
  status_choice <- status_lookup %>%
    select(status) %>%
    distinct() %>%
    pull
  
# users 
  users_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_users_lookup")
  users_lookup_list <- users_lookup %>%
    select(postcon_users) %>%
    pull
# review status
  review_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_review_status_lookup")
  review_status_list <- review_status_lookup %>%
    select(review_status)%>%
    pull
  
  
  

#system ids
  # comprehensive list
  system_id_all <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
    dplyr::arrange(system_id) %>%  
    dplyr::pull()
  
  # those with post-con status
  systems_pc <- odbc::dbGetQuery(poolConn, paste0("select * from fieldwork.tbl_postcon_status")) 
  system_id_postcon <- systems_pc %>% 
    select(system_id) %>%
    dplyr::arrange(system_id) %>%  
    dplyr::pull()
  
# Fiscal uarters 
  fq <- dbGetQuery(poolConn, "SELECT * FROM admin.tbl_fiscal_quarter_lookup")

  # filter out to more recent quarters
  q_list <- fq %>%
    arrange(fiscal_quarter_lookup_uid) %>%
    select(fiscal_quarter) %>%
    pull
  
# get the current fiscal year and list of all years from start of data (2012) to now
  current_fy <- lubridate::today() %m+% months(6) %>% year()
  start_fy <- 2012
  years <- start_fy:current_fy %>% sort(decreasing = TRUE)
  
  # #replace special characters with friendlier characters
  special_char_replace <- function(note){

    note_fix <- note %>%
      str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))

    return(note_fix)

  }
  
  # Define UI
  ui <- tagList(useShinyjs(), navbarPage("Post-Construction Status App", id = "TabPanelID", theme = shinytheme("cyborg"),
                   tabPanel("Post-Construction Status Table", value = "status", 
                            sidebarLayout(
                              
                              sidebarPanel(
                                #selectInput("system_id_pc", "System ID", choices = system_id_postcon, selected = NULL),
                                selectInput("date_range", "Date Range", choices = c("To-Date", "Fiscal Quarter")),
                                conditionalPanel(condition = "input.date_range == 'Fiscal Quarter'", 
                                                 fluidRow(column(12,
                                                                 selectInput("f_q", "Fiscal Quarter", choices = q_list, selected = "FY24Q2")))
                                ), 
                                selectInput("status", "Post Construction Status", choices = c("", status_choice) , selected = ""),
                                selectInput("review_status", "Review Status", choices = c("", review_status_list) , selected = ""),
                                selectInput("user", "Last Modified/Reviewed By", choices = c("", users_lookup_list) , selected = ""),
                                strong(""),
                                strong("Breakdown of Current Status"),
                                reactableOutput("summary_table"),
                              
                                
                                #1.3 DL Button --------
  
                                fluidRow(column(12, strong("Download all Post-Con Status Transactions and Notes"))),
                          
                                downloadButton("download_table", "Download All"),
                                
                                fluidRow(column(12, strong("Download Latest Status"))),
                                
                                downloadButton("download_latest", "Download Latest Status")
                                
                                
                              ),
          
                              mainPanel(
                                strong(span(textOutput("table_name"), style = "font-size:22px")),
                                reactableOutput("postcon_table") %>% withSpinner(color="#0dc5c1")
                              )
                            )
                            ),
                   tabPanel("Add/Edit Post-Construction Status", value = "add_edit", 
                   sidebarLayout(
                     
                     sidebarPanel(
                       selectInput("system_id", "System ID", choices = c("", system_id_all) , selected = ""),
                       conditionalPanel(condition = "input.create_status === false", selectInput("status_edit", "Post Construction Status", choices = c("", status_choice) , selected = "")),
                       hidden(checkboxInput("create_status","Create New Post-Con Status?",
                                     value = FALSE)),
                       selectInput("edit_review_status", "Review Status", choices = c("", review_status_list) , selected = ""),
                       selectInput("edit_user", "Last Modified/Reviewed By", choices = c("", users_lookup_list) , selected = ""),
                       conditionalPanel(condition = "input.create_status === true",
                                        textAreaInput("new_status", "New Post Construction Status:", height = '50px')),
                       selectInput("quarter_assigned", "Quarter", choices = c("", q_list) , selected = ""),
                       #dateInput("date",label = "Date",value = NULL),
                       textAreaInput("note", "Notes", height = '200px'),
                       disabled(actionButton("save_edit", "Save Changes")),
                       actionButton("clear_pcs", "Clear All Fields")
                       
                     ),
                     mainPanel(
                       conditionalPanel(condition = "input.system_id",
                                        h4(textOutput("current_header")),
                                        reactableOutput("sys_current_pc_table"),
                                        h4(textOutput("past_header")), 
                                        reactableOutput("sys_past_pc_table"))
                     )
                   )),
                   tabPanel("Quarterly QA", value = "qa", 
                            sidebarLayout(
                              sidebarPanel(
                              fluidRow(column(6,
                                              selectInput("fy", "Fiscal Year", choices = years)),
                                       column(6,
                                              selectInput("quarter", "Fiscal Quarter", 
                                                            choices = c("Q1", "Q2", "Q3", "Q4"))))

                           
                            ),
                            mainPanel(
                              h3(textOutput("qa_table_name")),
                              h3("Missing SRT or SRT Deployment Record this Quarter"),
                              reactableOutput("srt_qa_table"),
                              h3("Missing Post-Con Status for Systems with Post-Con SRT"),
                              reactableOutput("srt_nopostcon_table"),
                              h3("Missing Post-Con Status for Systems with CWL Data in the Database"),
                              reactableOutput("cwl_data_qa_table"),
                              h3("Missing Post-Con Status for SMPs with PPT Test this Quarter"),
                              reactableOutput("ppt_no_pc"),
                              h3("Missing Post-Con Status for Stormwater Trees with CET Test this Quarter"),
                              reactableOutput("cet_no_pc")

                              
                            ))
                            
                   )
  ),
  tags$style(HTML("
    #note {
      color: black !important;  /* Ensures text color is black */
      font-weight: bold !important;  /* Makes the text bold */
    }
  
  "))
  
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    #initialzie reactive values
    rv <- reactiveValues()
    
    #process text field to prevent sql injection
    rv$reason_step <- reactive(gsub('\'', '\'\'',  input$note))
    rv$input_note  <- reactive(special_char_replace(rv$reason_step()))
    
    

  
  
    # load required tables here
    #post-con status notes
    rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_notes"))
    
    #post-con status 
    rv$postcon_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status") %>%
                                    inner_join(fq_lookup, by = "fiscal_quarter_lookup_uid") %>%
                                    select(system_id, postcon_status_lookup_uid, status_date, postcon_status_uid, fiscal_quarter_lookup_uid,  fq = fiscal_quarter))
      
    
    #current status
    rv$postcon_status_current <- reactive(rv$postcon_status() %>%
      group_by(system_id) %>%
      dplyr::summarise(status_date = max(status_date)) %>%
      ungroup %>%
      inner_join(rv$postcon_status(), by = c("system_id"="system_id","status_date"="status_date")))
    
    
    # Most recent note of most recent stats
    rv$recent_notes <- reactive(rv$postcon_status_current() %>%
      inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
      select(system_id, note_date, notes, postcon_notes_uid) %>%
      arrange(desc(note_date)) %>%
      group_by(system_id) %>%
      summarise(notes = notes[1]))
    
    # Date of Most recent note of most recent stats
    rv$recent_notes_date <- reactive(rv$postcon_status_current() %>%
      inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
      select(system_id, note_date, notes, postcon_notes_uid) %>%
      arrange(desc(note_date)) %>%
      group_by(system_id) %>%
      summarise(note_date = note_date[1]))
    
    #post-con status types
    rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
    
    # join status 
    rv$postcon_status_dl <- reactive(rv$postcon_status() %>%
      inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
      inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
        select(system_id, status_date, status, note_date, notes))
    
    
    # latest status
    rv$latest_status_dl <- reactive(rv$postcon_status_current() %>%
                                      inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
                                      select(system_id, status_date, fq, status))
    
    # users 
    rv$last_user <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_users") %>%
                               inner_join(users_lookup, by = "postcon_users_uid"))
    
    # review status
    rv$review_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_review_status") %>%
                                   inner_join(review_status_lookup, by = "review_status_uid"))
    
    
    
    #create a date style for headers
    sf <- lubridate::stamp("March 1, 1999", orders = "%B %d, %Y")

    #Date range for QA tab
    #get quarters as dates
    rv$qa_start_quarter <- reactive(case_when(input$quarter == "Q3" ~ "1/1", 
                                           input$quarter == "Q4" ~ "4/1", 
                                           input$quarter == "Q1" ~ "7/1", 
                                           input$quarter == "Q2" ~ "10/1"))
    
    rv$qa_end_quarter <- reactive(case_when(input$quarter == "Q3" ~ "3/31", 
                                         input$quarter == "Q4" ~ "6/30", 
                                         input$quarter == "Q1" ~ "9/30", 
                                         input$quarter == "Q2" ~ "12/31"))
    
    
    #convert FY/Quarter to a real date for QA tab
    rv$qa_start_date <- reactive(lubridate::mdy(paste0(rv$qa_start_quarter(), "/", ifelse(input$quarter == "Q1" | input$quarter == "Q2", as.numeric(input$fy)-1,input$fy))))
    rv$qa_end_date <- reactive(lubridate::mdy(paste0(rv$qa_end_quarter(), "/", ifelse(input$quarter == "Q1" | input$quarter == "Q2", as.numeric(input$fy)-1,input$fy))))
    
    output$qa_table_name <- renderText(paste("Flagged SMPs for ","Fiscal Quarter", input$quarter, "of" , input$fy,"(", rv$qa_start_date(), " to ",  rv$qa_end_date(),")"))
    
    
    
    
    #get quarters as dates
    rv$postcon_start_quarter <- reactive(case_when(str_sub(input$f_q, 5, 7) == "Q3" ~ "1/1", 
                                                   str_sub(input$f_q, 5, 7) == "Q4" ~ "4/1", 
                                                   str_sub(input$f_q, 5, 7) == "Q1" ~ "7/1", 
                                                   str_sub(input$f_q, 5, 7) == "Q2" ~ "10/1"))
    
    rv$postcon_end_quarter <- reactive(case_when(str_sub(input$f_q, 5, 7) == "Q3" ~ "3/31", 
                                                 str_sub(input$f_q, 5, 7) == "Q4" ~ "6/30", 
                                                 str_sub(input$f_q, 5, 7) == "Q1" ~ "9/30", 
                                                 str_sub(input$f_q, 5, 7) == "Q2" ~ "12/31"))
    
    # parse the year component from this format "FY24Q2"
    rv$year <- reactive(str_sub(input$f_q, 3, 4))
    

    
    #convert FY/Quarter to a real date for postcon tab
    rv$postcon_start_date <- reactive(lubridate::mdy(paste0(rv$postcon_start_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year())-1, rv$year()))))
    rv$postcon_end_date <- reactive(lubridate::mdy(paste0(rv$postcon_end_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year())-1, rv$year()))))
    
    output$table_name <- renderText(ifelse(input$date_range == "To-Date", paste("Current Post-Con Status to Date:", input$status), paste("Current Post-Con Status", " Belonging to ", input$f_q,"; ","(", rv$postcon_start_date(), " to ",  rv$postcon_end_date(),")" , "; ",input$status, sep = "")))
    

  
### First tab: Post-Construction Status Table
    
    rv$review_status_filter <- reactive(
      if(input$review_status == "") {
        c(review_status_list, NA)
      } else{
        input$review_status
      }
    )
    
    rv$user_filter <- reactive(
      if(input$user == "") {
        c(users_lookup_list, NA)
      } else{
        input$user
      }
    )
    
    
    # todate 
    rv$pc_status_todate <- reactive(
      if(input$status == ""){
        
       rv$postcon_status_current() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
          left_join(rv$review_status(), by = "postcon_status_uid") %>%
          filter(postcon_users %in% rv$user_filter()) %>%
          filter(review_status %in% rv$review_status_filter()) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid) 

      } else{
        
       rv$postcon_status_current() %>%
          inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
          left_join(rv$review_status(), by = "postcon_status_uid") %>%
          filter(status == input$status) %>%
          filter(postcon_users %in% rv$user_filter()) %>%
          filter(review_status %in% rv$review_status_filter()) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid)
          
      }
    )
    
    # quarter based 
    rv$pc_status__q <- reactive( 
      if(input$status == "") {
        rv$postcon_status_current() %>%
          inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
          left_join(rv$review_status(), by = "postcon_status_uid") %>%  
          filter(fq == input$f_q) %>%
          filter(postcon_users %in% rv$user_filter()) %>%
          filter(review_status %in% rv$review_status_filter()) %>%  
          select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid)
          
      } else {
        
        rv$postcon_status_current() %>%
          inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
          left_join(rv$review_status(), by = "postcon_status_uid") %>%
          filter(fq == input$f_q)  %>%
          filter(status == input$status) %>%
          filter(postcon_users %in% rv$user_filter()) %>%
          filter(review_status %in% rv$review_status_filter()) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid)
        
      }
    )
    
    rv$pc_status <- reactive(ifelse(input$date_range == "To-Date", return(rv$pc_status_todate()), return(rv$pc_status__q())))

    # output$postcon_table <- renderReactable(
    #   reactable(rv$pc_status() %>% select(-postcon_status_uid), 
    #             theme = darkly(),
    #             fullWidth = TRUE,
    #             selection = "single",
    #             searchable = TRUE,
    #             onClick = "select",
    #             selectionId = "status_selected",
    #             #searchable = TRUE,
    #             showPageSizeOptions = TRUE,
    #             pageSizeOptions = c(25, 50, 100),
    #             defaultPageSize = 25,
    #             height = 1000,
    #             details = function(index) {
    #               nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$pc_status()$postcon_status_uid[index], ] %>%
    #                 arrange(desc(note_date)) %>%
    #                 select(`Date of Entry`= note_date, Notes = notes)
    #               htmltools::div(style = "padding: 1rem",
    #                              reactable(nested_notes, 
    #                                        theme = darkly(),
    #                                        columns = list(
    #                                `Date of Entry` = colDef(width = 150),
    #                                 Notes = colDef(width = 950)
    #                              ), outlined = TRUE)
    #               )
    #             }
    #             
    #   ))
    
    output$postcon_table <- renderReactable(
      reactable(
        rv$pc_status() %>% select(-postcon_status_uid),
        theme = darkly(),
        fullWidth = TRUE,
        selection = "single",
        searchable = TRUE,
        onClick = "select",
        selectionId = "status_selected",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        defaultPageSize = 25,
        height = 1000,
        columns = list(
          `Review Status` = colDef(
            style = function(value) {
              if (is.na(value)) {
                # Handling NA values; do nothing!
              } else if (value == "Complete") {
                return(list(background = "green", color = "white", fontweight = "bold"))
              } else if (value == "Needs Secondary Review") {
                return(list(background = "yellow", color = "black", fontweight = "bold"))
              } else if (value == "Incomplete") {
                return(list(background = "red", color = "white", fontweight = "bold"))
              } else {
                # Handle any unexpected values gracefully (default to white)
              }
            }
          )
        ),
        details = function(index) {
          nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$pc_status()$postcon_status_uid[index], ] %>%
            arrange(desc(note_date)) %>%
            select(`Date of Entry` = note_date, Notes = notes)
          htmltools::div(
            style = "padding: 1rem",
            reactable(
              nested_notes,
              theme = darkly(),
              columns = list(
                `Date of Entry` = colDef(width = 150),
                Notes = colDef(width = 950)
              ),
              outlined = TRUE
            )
          )
        }
      )
    )
    
    
    
    
    
    #download buttons
    # all notes and status transactions
    output$download_table <- downloadHandler(
      
      filename = function() {
        paste("Post-Con Table", "_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(filename){
        
        df_list <- list(rv$postcon_status_dl())
        write.xlsx(x = df_list , file = filename)
      }
    )
    
    # Only recent status 
    output$download_latest <- downloadHandler(
      
      filename = function() {
        paste("Latest_Status_Table", "_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(filename){
        
        df_list <- list(rv$latest_status_dl())
        write.xlsx(x = df_list , file = filename)
      }
    )
    
    
    
    
    
### Second tab: ADD/EDIT Post-Construction Status 
    
    # Create a reactiveVal to store the selected system_id
    selected_system_id <- reactiveVal(NULL)
    selected_status <- reactiveVal(NULL)
    selected_date <- reactiveVal(NULL)
    selected_note <- reactiveVal(NULL)
    selected_q <- reactiveVal(NULL)
    selected_user <- reactiveVal(NULL)
    selected_review_status <- reactiveVal(NULL)
  
    observeEvent(input$status_selected, {
      if (!is.null(input$status_selected)) {
        # Get the selected System ID from the clicked row
        selected_system_id(rv$pc_status()$`System ID`[input$status_selected])
        # selected_status(rv$pc_status()$`Post Construction Status`[input$status_selected])
        # selected_note(postcon_notes %>%
        #                 filter(postcon_status_uid == rv$pc_status()$postcon_status_uid[input$status_selected]) %>%
        #                 filter(note_date == max(note_date)) %>%
        #                 select(notes) %>%
        #                 pull)
        # Switch to the "Add/Edit Post-Construction Status" tab
        updateTabsetPanel(session, "TabPanelID", selected = "add_edit")
      }
    })

    #Modify the 'system_id' select input to use the reactiveVal
    observe({
      if (!is.null(selected_system_id())) {
        updateSelectInput(session, "system_id", selected = selected_system_id())
        # updateSelectInput(session, "status_edit", selected = selected_status())
        # updateSelectInput(session, "date", selected = Sys.Date())
        # updateTextAreaInput(session, "note", value = selected_note())
        
        
      }
    })
  
    #table header-current
    output$current_header <- renderText(
      paste("Current Post Construction Status for System ", input$system_id)
    )
    #table header-past
    output$past_header <- renderText(
      paste("Previous Post Construction Status for System ", input$system_id)
    )
    
    # current Post-con of a system
    rv$Current_sys_status <- reactive(
      
      rv$postcon_status_current() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
        left_join(rv$review_status(), by = "postcon_status_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid) %>%
        filter(`System ID` == input$system_id)
      
    )
    
    # all post-cons of a system
    rv$all_sys_status <- reactive(
      
      rv$postcon_status() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        left_join(rv$last_user(), by = "postcon_status_uid" ) %>%
        left_join(rv$review_status(), by = "postcon_status_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date of Entry` = status_date, Quarter = fq, `Last Modified/Reviewed` = postcon_users, `Review Status` = review_status, postcon_status_uid) %>%
        filter(postcon_status_uid %!in% rv$postcon_status_current()$postcon_status_uid) %>%
        filter(`System ID` == input$system_id) %>%
        arrange(desc(`Date of Entry`))
      
    )
    
    # current table 
    output$sys_current_pc_table <- renderReactable(
      reactable(rv$Current_sys_status()  %>%
                  select(-postcon_status_uid),
                theme = darkly(),
                fullWidth = TRUE,
                selection = "single",
                searchable = TRUE,
                onClick = "select",
                selectionId = "current_status_selected",
                #searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25,
                height = 400, 
                columns = list(
                  `Review Status` = colDef(
                    style = function(value) {
                      if (is.na(value)) {
                        # Handling NA values; do nothing!
                      } else if (value == "Complete") {
                        return(list(background = "green", color = "white", fontweight = "bold"))
                      } else if (value == "Needs Secondary Review") {
                        return(list(background = "yellow", color = "black", fontweight = "bold"))
                      } else if (value == "Incomplete") {
                        return(list(background = "red", color = "white", fontweight = "bold"))
                      } else {
                        # Handle any unexpected values gracefully (default to white)
                      }
                    }
                  )
                ),
                details = function(index) {
                  nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$Current_sys_status()$postcon_status_uid[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Date of Entry`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes,
                                           theme = darkly(),
                                           columns = list(
                                                          `Date of Entry` = colDef(width = 150),
                                                           Notes = colDef(width = 950)
                                 ), 
                                 outlined = TRUE)
                  )
                }
                
      ))
      
    # past table
    output$sys_past_pc_table <- renderReactable(
      reactable(rv$all_sys_status() %>% 
                  select(-postcon_status_uid),
                theme = darkly(),
                fullWidth = TRUE,
                selection = "single",
                searchable = TRUE,
                onClick = "select",
                selectionId = "past_status_selected",
                #searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25,
                height = 400, 
                columns = list(
                  `Review Status` = colDef(
                    style = function(value) {
                      if (is.na(value)) {
                        # Handling NA values; do nothing!
                      } else if (value == "Complete") {
                        return(list(background = "green", color = "white", fontweight = "bold"))
                      } else if (value == "Needs Secondary Review") {
                        return(list(background = "yellow", color = "black", fontweight = "bold"))
                      } else if (value == "Incomplete") {
                        return(list(background = "red", color = "white", fontweight = "bold"))
                      } else {
                        # Handle any unexpected values gracefully (default to white)
                      }
                    }
                  )
                ),
                details = function(index) {
                  nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$all_sys_status()$postcon_status_uid[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Date of Entry`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes, 
                                           theme = darkly(),
                                           columns = list(
                                                     `Date of Entry` = colDef(width = 150),
                                                      Notes = colDef(width = 950)
                                                       ), 
                                           outlined = TRUE)
                  )
                }
                
      ))
    
    # when a system id is selected 
    observeEvent(input$system_id, {

      updateSelectInput(session, "status_edit", selected = "")
      updateTextAreaInput(session, "note", value = "")
      updateTextAreaInput(session, "quarter_assigned", value = "")
      reset("create_status")
      reset("new_status")

      
        
    })
    
    # reset status if new status is being created
    observeEvent(input$create_status == TRUE,{
      reset("status_edit")
      disable("save_edit")
      })
    
    
    # system_id, data, and post_con status field can't remain blank
    observe(toggleState(id = "save_edit", (input$system_id != "" 
                                           & input$quarter_assigned != ""
                                           & input$edit_user != ""
                                           & input$edit_review_status != ""
                                           & (input$status_edit != "" |(input$create_status == TRUE & input$new_status !="" )))))
    
    
    #when a row any of the tables in add/edit is clicked
    observeEvent(input$current_status_selected, {
      
      if(rv$recent_notes_date() %>% filter(system_id == input$system_id) %>% nrow() > 0 ) {
        
      updated_date <- rv$recent_notes_date() %>%
          filter(system_id == input$system_id) %>%
          select(note_date) %>%
          pull
      } else {
        
      updated_date <- rv$Current_sys_status()$`Date of Entry`[input$current_status_selected]
      }
        
      #deselect from other tables
      updateReactable("sys_past_pc_table", selected = NA)
      selected_status(rv$Current_sys_status()$`Post Construction Status`[input$current_status_selected])
      selected_date(updated_date)
      selected_note(rv$recent_notes() %>%
                      filter(system_id == input$system_id) %>%
                      select(notes) %>%
                      pull) 
      # selected quarter
      selected_q(rv$Current_sys_status()$`Quarter`[input$current_status_selected])
      # selected user 
      selected_user(rv$Current_sys_status()$`Last Modified/Reviewed`[input$current_status_selected])
      # selected review status
      selected_review_status(rv$Current_sys_status()$`Review Status`[input$current_status_selected])
      
      
      updateSelectInput(session, "status_edit", selected = selected_status())
      updateTextAreaInput(session, "note", value = selected_note())
      updateSelectInput(session, "quarter_assigned", selected = selected_q())
      updateSelectInput(session, "edit_user", selected = selected_user())
      updateSelectInput(session, "edit_review_status", selected = selected_review_status())
    
      reset("create_status")
      reset("new_status")      

    })

    
    #when a row any of the tables in add/edit is clicked
    observeEvent(input$past_status_selected, {
      #deselect from other tables
      updateReactable("sys_current_pc_table", selected = NA)
      
      if(rv$postcon_notes() %>% filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>% nrow() > 0 ) {
        
        updated_date <- rv$postcon_notes() %>%
          filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>%
          arrange((desc(note_date))) %>%
          select(note_date) %>%
          pull %>%
          .[1]
      } else {
        
        updated_date <- rv$all_sys_status()$`Date of Entry`[input$past_status_selected]
      }
      
      
      
      # selected_system_id(rv$all_sys_status()$`System ID`[input$past_status_selected])
      selected_status(rv$all_sys_status()$`Post Construction Status`[input$past_status_selected])
      # selected_date(rv$all_sys_status()$`Date of Entry`[input$past_status_selected])
      selected_date(updated_date)
      selected_note(rv$postcon_notes() %>%
                      filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>%
                      arrange((desc(note_date))) %>%
                      select(notes) %>%
                      pull %>%
                      .[1])
      
      
      # selected quarter
      selected_q(rv$all_sys_status()$`Quarter`[input$past_status_selected])
      
      # selected user 
      selected_user(rv$all_sys_status()$`Last Modified/Reviewed`[input$past_status_selected])
      # selected review status
      selected_review_status(rv$all_sys_status()$`Review Status`[input$past_status_selected])
      
      # updateSelectInput(session, "system_id", selected = selected_system_id())
      updateSelectInput(session, "status_edit", selected = selected_status())
      updateTextAreaInput(session, "note", value = selected_note())
      updateSelectInput(session, "quarter_assigned", selected = selected_q())
      updateSelectInput(session, "edit_user", selected = selected_user())
      updateSelectInput(session, "edit_review_status", selected = selected_review_status())
      reset("create_status")
      reset("new_status")      
      
    })
    
    
    observeEvent(input$clear_pcs, {
      showModal(modalDialog(title = "Clear All Fields", 
                            "Are you sure you want to clear all fields on this tab?", 
                            modalButton("No"), 
                            actionButton("confirm_clear_pcs", "Yes")))
    })
    
    
    observeEvent(input$confirm_clear_pcs, {
      reset("system_id")
      reset("status_edit")
      reset("date")
      reset("note")
      reset("current_header")
      reset("sys_current_pc_table")
      reset("past_header")
      reset("sys_past_pc_table")
      reset("create_status")
      reset("new_status")
      reset("quarter_assigned")
      reset("edit_review_status")
      reset("edit_user")
      
      removeModal()
    })
    
    
    #add/edit button toggle
    rv$label <- reactive(if(length(input$current_status_selected) == 0 & length(input$past_status_selected) == 0) "Add New Post-Con Status Row" else "Edit Selected")
    observe(updateActionButton(session, "save_edit", label = rv$label()))
    
    
    
    
    ### On click "save_edit"
    
    observeEvent(input$save_edit, {
      
      
      
      # get the uid
      pc_uid <-  rv$postcon_status() %>%
        select(postcon_status_uid) %>%
        pull %>%
        max + 1
      
      if(length(input$current_status_selected) == 0 & length(input$past_status_selected) == 0){

          if(input$create_status == FALSE){
            
            pc_status_uid <-  rv$postcon_status_lookup() %>%
              filter(status == input$status_edit) %>%
              select(postcon_status_lookup_uid) %>%
              pull
            
          } else{
            
            pc_status_uid <- rv$postcon_status_lookup() %>%
              select(postcon_status_lookup_uid) %>%
              pull %>%
              max() + 1
            
            odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_postcon_status_lookup"), data.frame(status = input$new_status, postcon_status_lookup_uid = pc_status_uid), append= TRUE, row.names = FALSE )
           
            
          }
            
            
          rv$fiscal_quarter_uid <- reactive(fq_lookup %>%
              filter(fiscal_quarter == input$quarter_assigned) %>%
              select(fiscal_quarter_lookup_uid) %>%
              pull)
          
          rv$new_status <- reactive(data.frame(system_id = input$system_id,
                                   postcon_status_lookup_uid = pc_status_uid,
                                   status_date = Sys.Date(),
                                   postcon_status_uid = pc_uid,
                                   fiscal_quarter_lookup_uid = rv$fiscal_quarter_uid()))
          
          rv$new_note <- reactive(data.frame(note_date = Sys.Date(),
                                 notes =   rv$input_note(),
                                 postcon_status_uid = pc_uid))
          
          # get user id
          rv$user_uid <- reactive(
            filter(users_lookup, postcon_users == input$edit_user) %>%
              select(postcon_users_uid) %>% 
              pull
          )
          
          # get review status uid 
          rv$review_status_uid <- reactive(
            filter(review_status_lookup, review_status == input$edit_review_status) %>%
              select(review_status_uid) %>% 
              pull
          )
          
          #odbc::dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_status"), rv$new_status(), append= TRUE, row.names = FALSE )
          #rv$new_status_q <- reactive(paste0("INSERT INTO fieldwork.tbl_postcon_status (system_id, postcon_status_lookup_uid, status_date, fiscal_quarter_lookup_uid) VALUES ('", input$system_id, "',", pc_status_uid,",'",Sys.Date(), "',", rv$fiscal_quarter_uid(),")")) 
          
          rv$new_status_q <- reactive(paste0("INSERT INTO fieldwork.tbl_postcon_status (system_id, postcon_status_lookup_uid, status_date, postcon_status_uid, fiscal_quarter_lookup_uid) VALUES ('", input$system_id, "',", pc_status_uid,",'",Sys.Date(), "',", pc_uid, ",", rv$fiscal_quarter_uid(),")")) 
          odbc::dbGetQuery(poolConn, rv$new_status_q())
          
          rv$new_transaction_user <- reactive(paste0("INSERT INTO fieldwork.tbl_postcon_status_users (postcon_status_uid, postcon_users_uid) VALUES (", pc_uid,",", rv$user_uid(),")"))
          odbc::dbGetQuery(poolConn, rv$new_transaction_user())
          
          rv$new_transaction_reviewer <- reactive(paste0("INSERT into fieldwork.tbl_postcon_review_status (postcon_status_uid, review_status_uid) VALUES (", pc_uid, ",", rv$review_status_uid(),")"))
          odbc::dbGetQuery(poolConn, rv$new_transaction_reviewer())
          
          
          if( rv$input_note() !=""){
          #odbc::dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_notes"), rv$new_note(), append= TRUE, row.names = FALSE )
          rv$new_note_q <- reactive(paste0("INSERT INTO fieldwork.tbl_postcon_notes (note_date, notes, postcon_status_uid) VALUES ('", Sys.Date(), "','",  rv$input_note(), "',", pc_uid,")")) 
          odbc::dbGetQuery(poolConn, rv$new_note_q())  
            
          }
          
          # rerun queries
          #post-con status notes
          rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_notes"))
          
          #post-con status 
          rv$postcon_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status") %>%
                                          inner_join(fq_lookup, by = "fiscal_quarter_lookup_uid") %>%
                                          select(system_id, postcon_status_lookup_uid, status_date, postcon_status_uid, fiscal_quarter_lookup_uid,  fq = fiscal_quarter))
          # users 
          rv$last_user <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_users") %>%
                                     inner_join(users_lookup, by = "postcon_users_uid"))
          
          # review status
          rv$review_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_review_status") %>%
                                         inner_join(review_status_lookup, by = "review_status_uid"))
          
          
          
          #post-con status types
          rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
          
          reset("status_edit")
          reset("quarter_assigned")
          reset("note")
          reset("current_header")
          reset("sys_current_pc_table")
          reset("past_header")
          reset("sys_past_pc_table")
          reset("create_status")
          reset("new_status")
          reset("edit_review_status")
          reset("edit_user")
          reset("postcon_table")
          
          
          
          # update drop down choices 
          status_choice <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup") %>%
            select(status) %>%
            pull
          updateSelectInput(session, "status_edit", choices = c("", status_choice), selected = "")
          
        
      } else if(length(input$current_status_selected) != 0){
        # look for the status-if you want to create a new status, you have to write it in db first
          if(input$create_status == FALSE){
            
            pc_status_uid_current <-  rv$postcon_status_lookup() %>%
              filter(status == input$status_edit) %>%
              select(postcon_status_lookup_uid) %>%
              pull
            
          } else{
            
            pc_status_uid_current <- rv$postcon_status_lookup() %>%
              select(postcon_status_lookup_uid) %>%
              pull %>%
              max() + 1
            
            odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_postcon_status_lookup"), data.frame(status = input$new_status, postcon_status_lookup_uid = pc_status_uid_current), append= TRUE, row.names = FALSE )
          }
          
          pc_uid_current <- rv$Current_sys_status()$postcon_status_uid[input$current_status_selected]
          
          pc_notes_uid_current <- rv$postcon_status_current() %>%
                                             filter(system_id == input$system_id) %>%
                                             inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
                                             select(note_date, postcon_notes_uid) %>%
                                             arrange(desc(note_date)) %>%
                                             select(postcon_notes_uid) %>%
                                             pull %>%
                                             .[1]
          
          
          rv$fiscal_quarter_uid_edit <- reactive(fq_lookup %>%
                                              filter(fiscal_quarter == input$quarter_assigned) %>%
                                              select(fiscal_quarter_lookup_uid) %>%
                                              pull)
                                            
          
          edit_status_current <- paste0(
            "Update fieldwork.tbl_postcon_status SET system_id ='", input$system_id,"', postcon_status_lookup_uid = ", pc_status_uid_current,
            ", fiscal_quarter_lookup_uid =", rv$fiscal_quarter_uid_edit()," where postcon_status_uid = ", pc_uid_current)
            
          edit_note_current <- paste0("Update fieldwork.tbl_postcon_notes SET notes ='",  rv$input_note(),"', postcon_status_uid = ", pc_uid_current,
                                         " where postcon_notes_uid = ", pc_notes_uid_current)  
          
          
          odbc::dbGetQuery(poolConn, edit_status_current)
          
          
          # get user id
          rv$user_uid <- reactive(
            filter(users_lookup, postcon_users == input$edit_user) %>%
              select(postcon_users_uid) %>% 
              pull
          )
          
          # get review status uid 
          rv$review_status_uid <- reactive(
            filter(review_status_lookup, review_status == input$edit_review_status) %>%
              select(review_status_uid) %>% 
              pull
          )
          
          # edit user
          rv$edit_user_q <- reactive(paste0("Update fieldwork.tbl_postcon_status_users SET postcon_users_uid = ", rv$user_uid(), "where postcon_status_uid = ", pc_uid_current))
          odbc::dbGetQuery(poolConn, rv$edit_user_q())
          
          # edit review status
          rv$edit_review_status_q <- reactive(paste0("Update fieldwork.tbl_postcon_review_status SET review_status_uid = ", rv$review_status_uid(), "where postcon_status_uid = ", pc_uid_current))
          odbc::dbGetQuery(poolConn, rv$edit_review_status_q())
          
          
          if(!is.na(pc_notes_uid_current)){
            
          if( rv$input_note() !=""){
          odbc::dbGetQuery(poolConn, edit_note_current)
            cat(edit_note_current)
          } else {
            
            edit_note_current <- paste0("DELETE FROM fieldwork.tbl_postcon_notes where postcon_notes_uid = ", pc_notes_uid_current)  
            odbc::dbGetQuery(poolConn, edit_note_current)
            
          }
          } else {
            
            new_note_current <- paste0("INSERT INTO fieldwork.tbl_postcon_notes (note_date, notes, postcon_status_uid) VALUES ('", Sys.Date(),"','",  rv$input_note(),"',", pc_uid_current,")")  
            odbc::dbGetQuery(poolConn, new_note_current)
            cat(new_note_current)

          }
          
          # rerun queries
          #post-con status notes
          rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_notes"))

          #post-con status 
          rv$postcon_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status") %>%
                                          inner_join(fq_lookup, by = "fiscal_quarter_lookup_uid") %>%
                                          select(system_id, postcon_status_lookup_uid, status_date, postcon_status_uid, fiscal_quarter_lookup_uid,  fq = fiscal_quarter))
          
          # users 
          rv$last_user <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_users") %>%
                                     inner_join(users_lookup, by = "postcon_users_uid"))
          
          # review status
          rv$review_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_review_status") %>%
                                         inner_join(review_status_lookup, by = "review_status_uid"))
          
          #post-con status types
          rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
          
          reset("postcon_table")
          reset("status_edit")
          reset("quarter_assigned")
          reset("note")
          reset("current_header")
          reset("sys_current_pc_table")
          reset("past_header")
          reset("sys_past_pc_table")
          reset("create_status")
          reset("new_status")
          reset("edit_review_status")
          reset("edit_user")

          # update drop down choices 
          status_choice <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup") %>%
            select(status) %>%
            pull
          updateSelectInput(session, "status_edit", choices = c("", status_choice), selected = "")
          
        
        
      } else{

          if(input$create_status == FALSE){
            
            pc_status_uid_past <-  rv$postcon_status_lookup() %>%
              filter(status == input$status_edit) %>%
              select(postcon_status_lookup_uid) %>%
              pull
            
          } else{
            
            pc_status_uid_past <- rv$postcon_status_lookup() %>%
              select(postcon_status_lookup_uid) %>%
              pull %>%
              max() + 1
            
            odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_postcon_status_lookup"), data.frame(status = input$new_status, postcon_status_lookup_uid = pc_status_uid_past), append= TRUE, row.names = FALSE )
            
            
          }
          
          pc_uid_past <- rv$all_sys_status()$postcon_status_uid[input$past_status_selected]
          
          pc_notes_uid_past <- rv$postcon_notes() %>%
            filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>%
            arrange((desc(note_date))) %>%
            select(postcon_notes_uid) %>%
            pull %>%
            .[1]
            
          rv$fiscal_quarter_uid_edit <- reactive(fq_lookup %>%
                                                   filter(fiscal_quarter == input$quarter_assigned) %>%
                                                   select(fiscal_quarter_lookup_uid) %>%
                                                   pull)
            
          
          edit_status_past <- paste0(
            "Update fieldwork.tbl_postcon_status SET system_id ='", input$system_id,"', postcon_status_lookup_uid = ", pc_status_uid_past,
            ", fiscal_quarter_lookup_uid =", rv$fiscal_quarter_uid_edit()," where postcon_status_uid = ", pc_uid_past)
          
          edit_note_past <- paste0("Update fieldwork.tbl_postcon_notes SET notes ='",  rv$input_note() ,"', postcon_status_uid = ", pc_uid_past,
                                       " where postcon_notes_uid = ", pc_notes_uid_past)  
          
          
          odbc::dbGetQuery(poolConn, edit_status_past)
          
        
          # get user id
          rv$user_uid <- reactive(
            filter(users_lookup, postcon_users == input$edit_user) %>%
              select(postcon_users_uid) %>% 
              pull
          )
          
          # get review status uid 
          rv$review_status_uid <- reactive(
            filter(review_status_lookup, review_status == input$edit_review_status) %>%
              select(review_status_uid) %>% 
              pull
          )
          
          # edit user
          rv$edit_user_q <- reactive(paste0("Update fieldwork.tbl_postcon_status_users SET postcon_users_uid = ", rv$user_uid(), "where postcon_status_uid = ", pc_uid_past))
          odbc::dbGetQuery(poolConn, rv$edit_user_q())
          
          # edit review status
          rv$edit_review_status_q <- reactive(paste0("Update fieldwork.tbl_postcon_review_status SET review_status_uid = ", rv$review_status_uid(), "where postcon_status_uid = ", pc_uid_past))
          odbc::dbGetQuery(poolConn, rv$edit_review_status_q())
          
          
          if(!is.na(pc_notes_uid_past)){
          if( rv$input_note() !=""){
            odbc::dbGetQuery(poolConn, edit_note_past)
            cat(edit_note_past)
            
          } else {
            
            edit_note_past <- paste0("DELETE FROM fieldwork.tbl_postcon_notes where postcon_notes_uid = ", pc_notes_uid_past)  
            odbc::dbGetQuery(poolConn, edit_note_past)
            
          }
          } else{
            
            new_note_past <- paste0("INSERT INTO fieldwork.tbl_postcon_notes (note_date, notes, postcon_status_uid) VALUES ('", Sys.Date(),"','",  rv$input_note(),"',", pc_uid_past,")")  
            odbc::dbGetQuery(poolConn, new_note_past)
            cat(new_note_past)
            
            
          }
          
          # rerun queries
          #post-con status notes
          rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_notes"))
          
          #post-con status 
          rv$postcon_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status") %>%
                                          inner_join(fq_lookup, by = "fiscal_quarter_lookup_uid") %>%
                                          select(system_id, postcon_status_lookup_uid, status_date, postcon_status_uid, fiscal_quarter_lookup_uid,  fq = fiscal_quarter))
          
          # users 
          rv$last_user <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_users") %>%
                                     inner_join(users_lookup, by = "postcon_users_uid"))
          
          # review status
          rv$review_status <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_review_status") %>%
                                         inner_join(review_status_lookup, by = "review_status_uid"))
          
          #post-con status types
          rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
          
          reset("postcon_table")
          reset("status_edit")
          reset("quarter_assigned")
          reset("note")
          reset("current_header")
          reset("sys_current_pc_table")
          reset("past_header")
          reset("sys_past_pc_table")
          reset("create_status")
          reset("new_status")
          reset("edit_review_status")
          reset("edit_user")
          
          
          # update drop down choices 
          status_choice <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup") %>%
            select(status) %>%
            pull
          updateSelectInput(session, "status_edit", choices = c("", status_choice), selected = "")
        
      }
      
      updateReactable("sys_current_pc_table", selected = NA)
      updateReactable("sys_past_pc_table", selected = NA)
      
    }
    )
    
    
### Summary page
    
    rv$summary <- reactive({
      
      status_break <- rv$postcon_status_current() %>%
                             inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
                             select(system_id, Status = status) %>%
                             group_by(Status) %>%
                             summarise(Count = n()) %>%
                             arrange(Count)
      
      
      total_row <- data.frame(Status = "Total", Count = sum(status_break$Count))
      return(bind_rows(status_break, total_row))
                                
      })
    
    output$summary_table <- renderReactable(
      
      reactable(rv$summary(), 
                theme = cyborg(),
                rowStyle = list(color = "white"),
                searchable = FALSE,
                pagination = FALSE,
                sortable = FALSE,
                striped = TRUE,
                filterable = FALSE, 
                fullWidth = TRUE) 

    )
    
    ### QA TAB
    
    # greenit to get the smp-type
    smpbdv_df <- dbGetQuery(poolConn,"SELECT system_id, smp_smptype as smp_type FROM external.tbl_smpbdv")
    
    # postcon
    systems_pc <- odbc::dbGetQuery(poolConn, paste0("select * from fieldwork.tbl_postcon_status")) 
    
    # srt
    rv$srt_postcon <- reactive(dbGetQuery(poolConn,  paste0("SELECT * FROM fieldwork.tbl_srt where system_id like '%-%' and con_phase_lookup_uid = 2 and test_date < '", rv$qa_end_date(), "'", sep = "")))
    rv$srt_Q <- reactive(paste0("SELECT * FROM fieldwork.tbl_srt WHERE test_date >= ", "'", rv$qa_start_date(), "'", " AND test_date <= '", rv$qa_end_date(),"'" , "AND system_id like '%-%'", sep = ""))
    rv$srt <- reactive(dbGetQuery(poolConn,  rv$srt_Q()))
    
    # deployments
    deployment_all <- dbGetQuery(poolConn, "SELECT *, admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.viw_deployment_full where type = 'LEVEL' AND smp_id like '%-%-%'")
    
    #porous pavement test
    ppt <- dbGetQuery(poolConn, "SELECT *, admin.fun_smp_to_system(smp_id) as system_id from fieldwork.tbl_porous_pavement where smp_id like '%-%-%'") %>%
      mutate(test_type = "PPT") %>%
      distinct()
    
    #cet
    cet <- dbGetQuery(poolConn, "SELECT * from fieldwork.tbl_capture_efficiency where system_id like '%-%'") %>%
      mutate(test_type = "CET") %>%
      inner_join(smpbdv_df, by ="system_id") %>%
      filter(smp_type == "Stormwater Tree") %>%
      distinct()
    cet$test_date <- as.Date(cet$test_date)
    

    
    
    ow <- dbGetQuery(poolConn, "SELECT DISTINCT admin.fun_smp_to_system(smp_id) as system_id, ow_suffix, ow_uid
                                                FROM fieldwork.tbl_ow WHERE smp_id like '%-%-%'")
    
    ow_with_data <- dbGetQuery(poolConn, "SELECT DISTINCT ow_uid FROM data.mat_level_data_day")
    
      
    cwl_data_list <- ow_with_data %>%
      inner_join(ow, by = "ow_uid") %>%
      select(system_id) %>%
      distinct()
    
    # CWL data
    # cwl_data_list <- dbGetQuery(poolConn, "WITH cte_smp_id_ow AS (
    #                                             SELECT DISTINCT admin.fun_smp_to_system(smp_id) as system_id, ow_suffix, ow_uid
    #                                             FROM fieldwork.tbl_ow
    #                                             ),
    #                                             cte_CWL_uid AS (
    #                                             SELECT DISTINCT ow_uid
    #                                             FROM data.tbl_ow_leveldata_raw
    #                                             )
    #                                             SELECT DISTINCT system_id
    #                                             FROM cte_CWL_uid AS l
    #                                             INNER JOIN cte_smp_id_ow AS r
    #                                             ON l.ow_uid = r.ow_uid
    #                                             WHERE system_id like '%-%'
    #                                             ")
    # 
    ### Matching SRT deployments with SRT table records to ensure they match (full join between srt and deployment table)
    rv$srt_qa <- reactive(deployment_all %>%
                            filter(deployment_dtime <= rv$qa_end_date() & deployment_dtime >= rv$qa_start_date()) %>%                   
                            filter(term == "SRT") %>%
                            full_join(rv$srt(), by = c("deployment_dtime" = "test_date", "system_id")) %>%
                            filter(is.na(srt_uid) | is.na(deployment_uid)) %>%
                            mutate(deployment_dtime = deployment_dtime %>% lubridate::ymd()) %>%
                            distinct() %>%
                            select(`System ID` = system_id, `SMP ID` = smp_id, `Deployment ID` = deployment_uid, ` SRT ID` = srt_uid, `Deployment/SRT Date` = deployment_dtime))

    ### left join between postcon srt table and the post-con status table to ensure all those systems with postcon SRT have post-con status
    rv$srt_nopostcon <- reactive(rv$srt_postcon() %>%
                                   left_join(systems_pc, by = "system_id") %>%
                                   filter(is.na(postcon_status_uid)) %>%
                                   mutate(`SRT Quarter` = paste(input$fy, input$quarter, sep = "")) %>%
                                   select(`System ID` = system_id, `SRT Quarter`, `Post-Con Status ID`= postcon_status_uid) %>%
                                   distinct()
    )
    
    
    #### ALl systems with cwl data in DB must have postcon status
    rv$cwl_data_qa <- reactive(cwl_data_list %>%
                                 left_join(systems_pc, by = "system_id") %>%
                                 filter(is.na(postcon_status_uid)) %>%
                                 select(`System ID` = system_id, `Post-Con Status ID`= postcon_status_uid) %>%
                                 distinct())

                                     
    # PPT during the the quarter performed, but no post-con status
    rv$ppt_no_pc <- reactive(ppt %>% 
                               filter(test_date <= rv$qa_end_date() & test_date > rv$qa_start_date()) %>%
                               left_join(systems_pc, by = "system_id") %>%
                               filter(is.na(postcon_status_uid)) %>%
                               mutate(test_date = test_date %>% lubridate::ymd()) %>%
                               select(`SMP ID` = smp_id, `Test Date` = test_date) %>%
                               distinct())
    
    # CET during the the quarter performed, but no post-con status
    rv$cet_no_pc <- reactive(cet %>% 
                               filter(test_date <= rv$qa_end_date() & test_date > rv$qa_start_date()) %>%
                               left_join(systems_pc, by = "system_id") %>%
                               filter(is.na(postcon_status_uid)) %>%
                               mutate(test_date = test_date %>% lubridate::ymd()) %>%
                               select(`System ID` = system_id, `Test Date` = test_date) %>%
                               distinct())
                                       
    
    
    # SRT
    output$srt_qa_table <- renderReactable(
      reactable(rv$srt_qa(),
                theme = darkly(),)
    )
    
    # SRT no postcon
    output$srt_nopostcon_table <- renderReactable(
      reactable(rv$srt_nopostcon(),
                theme = darkly(),)
    )

    #CWL Data
    output$cwl_data_qa_table <- renderReactable(
      reactable(rv$cwl_data_qa(),
                theme = darkly(),)
    )
    
    
    #PPT no PC
    output$ppt_no_pc <- renderReactable(
      reactable(rv$ppt_no_pc(),
                theme = darkly(),)
    )
    
    #Stormwater Tree with CET but no PC
    output$cet_no_pc <- renderReactable(
      reactable(rv$cet_no_pc(),
                theme = darkly(),)
    )
    
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  

  

  
  
  