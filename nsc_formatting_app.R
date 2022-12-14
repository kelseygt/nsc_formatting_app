# The below is a Shiny webapp that takes a raw data pull from the Banner student information
# system and formats it for upload to the National Student Clearinghouse.

# Make sure your Excel dates are in mm/dd/yyyy format before uploading (rather than 2-Feb format)

library(shiny)
library(tidyverse)
library(lubridate)
library(stringi)

# Define UI 
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file_to_upload", "Choose CSV file: ", accept = ".csv"),
      selectInput("query_option", "Select your query option: ", choices = c("CO", "DA", "PA", "SE")),
      actionButton("format_button", "Format"),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server 
server <- function(input, output) {
  
  raw_data <- eventReactive(input$file_to_upload, {
    read_csv(input$file_to_upload$datapath)
  })
  
  formatted_data <- eventReactive(input$format_button, {
    formatted_uploaded_file <- raw_data() %>%
      mutate(
        D1 = rep("D1", times = length(SPRIDEN_PIDM)),                                            # creates the repeating D1 column
        ssn = rep("", times = length(SPRIDEN_PIDM)),                                             # creates an empty column for the ssn we don't include
        first_name = stri_trans_general(SPRIDEN_FIRST_NAME, 'latin-ascii'),                      # removes all punctuation in the first name, and converts special characters to non-special characters
        middle_initial = stri_trans_general(substr(SPRIDEN_MI, 0, 1), 'latin-ascii'),            # extracts just the first initial of an individual's middle name, and converts special characters to non-special characters
        last_name = stri_trans_general(SPRIDEN_LAST_NAME, 'latin-ascii'),                        # removes all punctuation in the last name, and converts special characters to non-special characters
        suffix = rep("", times = length(SPRIDEN_PIDM)),                                          # creates an empty column for suffix, which we don't provide
        bday = gsub("[[:punct:]]", "", ymd(as_date(SPBPERS_BIRTH_DATE, format = "%m/%d/%Y"))),   # formats date of birth correctly
        search_date = gsub("[[:punct:]]", "", ymd(as_date(SEARCH_DATE, format = "%m/%d/%Y"))),   # formats search date correctly
        blank_column = rep("", times = length(SPRIDEN_PIDM)),                                    # creates the required blank column
        school_code = rep("001360", times = length(SPRIDEN_PIDM)),                               # creates the school code column
        branch_code = rep("00", times = length(SPRIDEN_PIDM)),                                   # creates the branch code column
        id_pidm = paste0(SPRIDEN_ID, "_", SPRIDEN_PIDM)                                          # creates a unique identifier, a concatenation of pidm and 900 num
      )
    
    formatted_uploaded_file$middle_initial[is.na(formatted_uploaded_file$middle_initial)] <- ""
    
    organized_format <- select(formatted_uploaded_file, c('D1',
                                                          'ssn',
                                                          'first_name',                                                      
                                                          'middle_initial',
                                                          'last_name',
                                                          'suffix',
                                                          'bday',
                                                          'search_date',
                                                          'blank_column',
                                                          'school_code',
                                                          'branch_code',
                                                          'id_pidm'))
    
    header_row <- c("H1", "001360", "00", "Metropolitan State University of Denver", 
                    gsub("[[:punct:]]", "", Sys.Date()), input$query_option, "I", rep(NA, times = 5))
    
    trailer_row <- c("T1", as.character(length(raw_data()$SPRIDEN_PIDM) + 2), rep(NA, times = 13))
    
    completed_format <- as.data.frame(rbind(header_row, organized_format, trailer_row))
    
    completed_format[is.na(completed_format)] <- ""
    
    completed_format
    
  })
  
  output$table <- renderTable({
    formatted_data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nsc_upload_data_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      formatted_data() %>%
        write_delim(file, delim = "\t", col_names = F)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)

# See SQL fill for base pull (just join on your target population)

# And add column with the header SEARCH_DATE for the search date in question in mm/dd/yyyy format.