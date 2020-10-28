library(shiny)

# which fields get saved 
fieldsAll <- c("Name", "EMail", "Registration_ID")

# which fields are mandatory
fieldsMandatory <- c("Registration_ID")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp human format YYYYMMDD_HHMMSS
humanTime <- function() {
  format(Sys.time(), "%Y%m%d%_%H%M%OS")
}

timezonefactor <- function() {
  if( Sys.timezone()=="Asia/Calcutta") return(0) else return(5.5*60*60)
}

humanTime2 <- function() {
   format(Sys.time()+timezonefactor(), "%d-%m-%y %H:%M:%OS")
}

humanTime3 <- function() {
  format(Sys.time()+timezonefactor(), "%y%m%d")
}

regExists <- function(data) {
  reg_ids = loadData()$Registration_ID
  data_exists =c(data[3]) %in% reg_ids
  return(data_exists)
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s.csv",humanTime())
  data_exists=regExists(data)
  if(data_exists){
      shinyjs::show("exists_msg")
      }
  else{
    write.csv(x = data, file = file.path(responsesDir(), fileName),
              row.names = FALSE, quote = TRUE)
    
    shinyjs::show("thankyou_msg")
  }
  
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir()), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}
# directory where responses get stored
responsesDir <- function() {
  res_path = file.path("responses",humanTime3())
  dir.create(res_path, recursive = TRUE, showWarnings = FALSE)
  print(res_path)
  return(res_path)
}


# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof")



shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Attendance Form",
    div(id = "header",
        h1("Attendance Form using Shiny"),
        strong( 
          span("Created by "),
          a("Jaykumar")
        )
    ),
    
    fluidRow(
      column(6,
             div(
               id = "form",
               
               textInput("Name", "Name"),
               textInput("EMail", "E-Mail ID"),
               textInput("Registration_ID", labelMandatory("Registration ID")),
               actionButton("submit", "Submit", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!")
                 ),
               div(
                 id = "exists_msg",
                 h3("Attendance Already Registered!")
                 
               )
             )
      ),
      column(6,
             uiOutput("adminPanelContainer")
      )
    )
  ),
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) toupper(input[[x]]))
      data <- c(data, Submit_Date=humanTime2(),timestamp=epochTime())
      data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        shinyjs::reset("form")
        shinyjs::hide("form")
        saveData(formData())
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    
    
    # render the admin panel
    output$adminPanelContainer <- renderUI({
      if (!isAdmin()) return()
      
      div(
        id = "adminPanel",
        h2("Attendance Dashboard"),
        downloadButton("downloadBtn", "Download responses"), br(), br(),
        DT::dataTableOutput("responsesTable"), br(),
        ""
      )
    })
    
    # determine if current user is admin
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })    
    
    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data <- data[c("Name","EMail","Registration_ID","Submit_Date")]
      #data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })
    
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() { 
        sprintf("Attendace_Data_%s.csv", humanTime())
      },
      content = function(file) {
        data = loadData()
        data = data[c("Name","EMail","Registration_ID","Submit_Date")]
        write.csv(data, file, row.names = FALSE)
      }
    )    
  }
)
