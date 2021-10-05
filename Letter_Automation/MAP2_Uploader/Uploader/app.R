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

source("/srv/shiny-server/previsit_uploader.R")
source("/srv/shiny-server/ty_uploader.R")
source("/srv/shiny-server/fb_uploader.R")
source("/srv/shiny-server/LP_uploader.R")

library(shiny)
library(flextable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Participant Letter Generator 2.0"),
    textInput(inputId = "n", "VMAC ID", ""),
    numericInput(inputId = "d", "Epoch", value=5),
    selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP","Thank You"="ty","Previsit"="previsit")),
    actionButton(inputId = "submit",label = "Submit"),
    textOutput(outputId = "d")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #re <- reactive({
    re <- eventReactive( input$submit, {
            letter <- input$l
            epoch <- input$d
            vmac <- input$n
            
            vmac <- unlist(strsplit(vmac, ","))
            
            
            #check <- input$check
            
            #if (check) {
                # Run uploader scripts that loop through each ptp
            #}
            for (vmac_int in vmac) {
                print(vmac_int)
                command <- paste0(letter,"_uploader(",epoch,",",vmac_int,")")
                print(command)
                eval(parse(text = command))
                letter_conv <- c("fb"="Feedback", "LP"="LP Previsit","ty"="Thank You","previsit"="Previsit")
                lett <- letter_conv[letter]
            }
            confirmation <<- paste0(lett," Letter for VMAC ID: ",input$n," has been generated!")
        }
    )
    #})
    output$d <-
        renderText({
            re()
            #remove(list = ls())
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
