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

source("~/Letter_Auto/TAP_Uploader/previsit_uploader.R")
source("~/Letter_Auto/TAP_Uploader/ty_uploader.R")
source("~/Letter_Auto/TAP_Uploader/fb_uploader.R")
source("~/Letter_Auto/TAP_Uploader/LP_uploader.R")
source("~/Letter_Auto/TAP_Uploader/np_uploader.R")
source("~/Letter_Auto/TAP_Uploader/pv_uploader.R")
source("~/Letter_Auto/TAP_Uploader/cond_uploader.R")

library(shiny)
library(flextable)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("TAP Letter Generator"),
    tabsetPanel(
        tabPanel("Letters", fluid = TRUE,
                 textInput(inputId = "n", "Record ID", ""),
                 numericInput(inputId = "d", "Epoch", value=1),
                 selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP","Thank You"="ty","Previsit"="pv", "Condolence"= "cond")),
                 actionButton(inputId = "submit",label = "Submit"),
                 textOutput(outputId = "d"),
                 textOutput(outputId = "error")
        ),
        tabPanel("Tables", fluid = TRUE,
                 textInput(inputId = "vmac", "Record ID", ""),
                 numericInput(inputId = "epoch", "Epoch", value=1),
                 selectInput(inputId = "type", "Type of Table", choices = c("Neuropsych"="np")),
                 actionButton(inputId = "submit2",label = "Submit"),
                 dataTableOutput(outputId = "ses"),
                 #textOutput("exp1")
        )
    )
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
            print(command)
            err <- eval(parse(text = command))
            letter_conv <- c("fb"="Feedback", "LP"="LP Previsit","ty"="Thank You","pv"="Previsit","cond"="Condolence")
            lett <- letter_conv[letter]
            conf <- ""
            if (err=="") {conf <- paste0(lett," Letter for VMAC ID: ",input$n," has been generated!")} else {conf <- "An error has occurred."}
        }
        rea <<- list(confirm = conf,error = err)
        
    }
    )
    
    output$d <-
        renderText({
            rea <- re()
            rea$confirm
        })
    output$error <-
        renderText({
            rea <- re()
            rea$error
        })
    re2 <- eventReactive( input$submit2, {
        tab <- input$type
        epoch <- input$epoch
        vmac <- input$vmac
        
        command <- paste0(tab,"_uploader(",epoch,",",vmac,")")
        
        dat <- eval(parse(text = command))
        
        results <- data.frame(
            "Epoch" = c("Baseline"),
            dat
        )
        
        results <<- as.matrix(results)
        return(results)
        
    })
    output$ses <- renderDataTable({re2()})
    #output$exp1 <- renderText("Table has been Generated")
}

# Run the application 
shinyApp(ui = ui, server = server)
