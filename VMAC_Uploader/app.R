#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.port = 5001)
options(shiny.host = '127.0.0.1')

source("~/Letter_Automation/VMAC_Uploader/vmac_fu_uploader.R")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Registry Letter Generator"),
    textInput(inputId = "n", "Screening ID", ""),
    #numericInput(inputId = "d", "Epoch", value=5),
    #selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP", "MAP Previsit Letter"="MAP","Thank You"="ty")),
    #submitButton("Generate Document", icon("refresh")),
    
    #checkboxInput(inputId = "check", label = "Make letter for all"),
    
    actionButton(inputId = "submit",label = "Submit"),
    textOutput(outputId = "d")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #re <- reactive({
    re <- eventReactive( input$submit, {
        #letter <- input$l
        #epoch <- input$d
        vmac <- input$n
        #check <- input$check
        
        vmac <- unlist(strsplit(vmac, ","))
        print(vmac)
        
        #if (check) {
        # Run uploader scripts that loop through each ptp
        #}
        for (vmac_i in vmac) {
            command <- paste0("vmac_fu_uploader(",vmac_i,")")
            eval(parse(text = command))
            #letter_conv <- c("fb"="Feedback", "LP"="LP Previsit", "MAP"="MAP Previsit","ty"="Thank You")
            lett <- "Registry"
        }
        confirmation <<- paste0(lett," Letter for MAP ID: ",vmac," has been generated!")
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
