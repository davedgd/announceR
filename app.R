doInstall <- FALSE

if (doInstall)
  devtools::install_github("paulc91/shinyauthr")

library(shiny)
library(readxl)
library(purrr)
library(dplyr)
library(stringr)
library(magrittr)
library(googleLanguageR)
library(rstudioapi)
library(shinyauthr)
library(shinyjs)

doAuth <- TRUE

user_base <- data.frame(
  user = c("demo"),
  password = c("demo"), 
  permissions = c("standard"),
  name = c("Demo"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# -----
# Setup
# -----

setwd("~/Dropbox/GitHub/announceR")
gl_auth("~/Dropbox/GitHub/API/app.json") # see ?googleLanguageR for instructions on how to set API key via JSON file

options(shiny.host = "0.0.0.0") # use 0.0.0.0 to make available online (or on LAN); otherwise localhost
options(shiny.port = 1234) # set port

voiceOptions <-
  gl_talk_languages(languageCode = "en-US") %>%
  mutate(label = paste(name, " (", languageCodes, ", ", ssmlGender, ")", sep = ""))

voiceOptions

ui <- fluidPage(
  title = "announceR",
  useShinyjs(),
  if (doAuth) shinyauthr::loginUI(id = "login",
                                                    title = "Welcome",
                                                    login_title = "Log In"),
                div(id = "controls", 
                    style = "display: none;",
                    fluidRow(br(),
                             column(
                  width = 4,
                  fileInput(
                    inputId = "excelInput",
                    label = "Load Excel (XLSX) File",
                    multiple = FALSE,
                    accept = c(".xlsx")
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "voice",
                    label = "Select Voice",
                    choices = setNames(as.list(voiceOptions$name), voiceOptions$label),
                    selected = "en-US-Wavenet-D"
                  )
                )),
                fluidRow(column(width = 12,
                                uiOutput("audio"))))
)

server <- function (input, output, session) {
  
  if (doAuth) {
    
    logout_init <- callModule(shinyauthr::logout, 
                              id = "logout", 
                              active = reactive(credentials()$user_auth))
    
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init()))
    
    runjs('document.querySelector("#login-password").addEventListener("keyup", event => {
            if (event.key !== "Enter") return;
            document.querySelector("#login-button").click();
            event.preventDefault();
          });') # allow hitting enter to submit a password
    
    observeEvent(credentials(), {
      
      if (doAuth) req(credentials()$user_auth)
        show(id = "controls")
      
    })
    
  }
  
  observeEvent(input$excelInput, {
    
    if (doAuth) req(credentials()$user_auth)
    
    dat <- read_xlsx(input$excelInput$datapath)
    
    dat <- dat %>%
      mutate(
        FirstNameReplace = ifelse(is.na(PhoneticFirstName), FirstName, PhoneticFirstName),
        LastNameReplace  = ifelse(is.na(PhoneticLastName),  LastName,  PhoneticLastName),
        ID = paste0("ID", row_number())
      )
    
    readDat <- dat %>%
      mutate(
        Transcript = str_replace(
          Script,
          "FULLNAME",
          paste(FirstNameReplace, LastNameReplace)
        ),
        Text = str_replace(Script, "FULLNAME", paste(FirstName, LastName))
      )
    
    output$audio <- renderUI({
      
      if (doAuth) req(credentials()$user_auth)
      
      theTags <- list()
      
      for (each in 1:nrow(readDat))
        theTags[[each]] <-
          tags$tr(tags$td(paste(
            readDat$FirstName[each], readDat$LastName[each]
          )),
          tags$td(gl_talk_shinyUI(readDat$ID[each])),
          tags$td(readDat$Text[each]))
      
      tagList(tags$table(style = "border-spacing: 6px; border-collapse: separate;", tags$thead(
        tags$tr(
          tags$th("Name"),
          tags$th("Audio"),
          tags$th("Transcript")
        )
      ),
      theTags))
      
    })
    
    lapply(1:nrow(readDat), function (x) {
      callModule(
        module = gl_talk_shiny,
        id = readDat$ID[x],
        name = input$voice,
        #gender = "FEMALE",
        #languageCode = "en-US",
        audioEncoding = "LINEAR16",
        volumeGainDb = 10,
        speakingRate = 1,
        pitch = 0,
        transcript = reactive({
          readDat$Transcript[x]
        }),
        autoplay = FALSE,
        keep_wav = TRUE
      )
    })
    
  observeEvent({
    input$voice
    input$excelInput
  }, {
    
    if (!is.null(input$excelInput$datapath))
      lapply(1:nrow(readDat), function (x) {
        callModule(
          module = gl_talk_shiny,
          id = readDat$ID[x],
          name = input$voice,
          #gender = "FEMALE",
          #languageCode = "en-US",
          audioEncoding = "LINEAR16",
          volumeGainDb = 10,
          speakingRate = 1,
          pitch = 0,
          transcript = reactive({
            readDat$Transcript[x]
          }),
          autoplay = FALSE,
          keep_wav = TRUE
        )
      })
    
  })
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)