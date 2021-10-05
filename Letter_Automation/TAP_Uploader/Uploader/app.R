#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.port = 5000)
options(shiny.host = '127.0.0.1')

source("~/TAP Uploader/previsit_uploader.R")
source("~/TAP Uploader/ty_uploader.R")
source("~/TAP Uploader/fb_uploader.R")
source("~/TAP Uploader/LP_uploader.R")

library(shiny)
library(flextable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Participant Letter Generator"),
    textInput(inputId = "n", "Record ID", ""),
    numericInput(inputId = "d", "Epoch", value=1),
    selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP", "Previsit Letter"="previsit","Thank You"="ty")),
    actionButton(inputId = "submit",label = "Submit"),
    textOutput(outputId = "d")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #re <- reactive({
    re <- eventReactive( input$submit, {
            letter <- input$l
            epoch <- input$d
            records <- input$n
            records <- unlist(strsplit(records, ","))
            
            for (record in records) {
                print(record)
                command <- paste0(letter,"_uploader(",epoch,",",record,")")
                print(command)
                eval(parse(text = command))
                letter_conv <- c("fb"="Feedback", "LP"="LP Previsit", "previsit"="Previsit","ty"="Thank You")
                lett <- letter_conv[letter]
            }
            
            confirmation <<- paste0(lett," Letter for Record ID: ",input$n," has been generated!")
        }
    )
    #})
    output$d <- renderText({re()})
}

# Run the application 
shinyApp(ui = ui, server = server)
