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

source("~/Letter_Automation/MAP_Uploader/MAP_uploader.R")
source("~/Letter_Automation/MAP_Uploader/previsit_uploader.R")
source("~/Letter_Automation/MAP_Uploader/ty_uploader.R")
source("~/Letter_Automation/MAP_Uploader/fb_uploader.R")
source("~/Letter_Automation/MAP_Uploader/LP_uploader.R")
#source("~/Letter_Automation/MAP_Uploader/fb_avail.R")

library(shiny)
library(flextable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Participant Letter Generator"),
    textInput(inputId = "n", "VMAC ID", ""),
    numericInput(inputId = "d", "Epoch", value=5),
    selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP", "Previsit Letter"="MAP","Thank You"="ty","Revamped Previsit"="previsit")),
    actionButton(inputId = "submit",label = "Submit"),
    textOutput(outputId = "d")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    re <- eventReactive( input$submit, {
            letter <- input$l
            epoch <- input$d
            vmac <- input$n
            
            vmac <- unlist(strsplit(vmac, ","))
            
            for (vmac_int in vmac) {
                print(vmac_int)
                command <- paste0(letter,"_uploader(",epoch,",",vmac_int,")")
                #command2 <- paste0(letter,"_avail(",epoch,",",vmac_int,")")
                print(command)
                #print(command2)
                #eval(parse(text = command2))
                eval(parse(text = command))
                letter_conv <- c("fb"="Feedback", "LP"="LP Previsit", "MAP"="Previsit","ty"="Thank You","previsit"="Previsit")
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
