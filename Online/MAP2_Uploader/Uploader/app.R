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
source("/srv/shiny-server/pv_uploader.R")
source("/srv/shiny-server/ty_uploader.R")
source("/srv/shiny-server/fb_uploader.R")
source("/srv/shiny-server/LP_uploader.R")
source("/srv/shiny-server/np_uploader.R")
source("/srv/shiny-server/blood_uploader.R")
source("/srv/shiny-server/brain_uploader.R")
source("/srv/shiny-server/extra_uploader.R")
source("/srv/shiny-server/echo_uploader.R")

library(shiny)
library(flextable)

ui <- fluidPage(
    
    # Application title
    titlePanel("Letter Generator 2.0"),
    tabsetPanel(
        tabPanel("Letters", fluid = TRUE,
                 textInput(inputId = "n", "VMAC ID", ""),
                 numericInput(inputId = "d", "Epoch", value=5),
                 selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP","Thank You"="ty","Previsit"="previsit","Full Previsit"="pv","Bloodwork Incidental"="blood","Brain Incidental"="brain","Extracardiac Incidental"="extra","Echo Incidental"="echo")),
                 actionButton(inputId = "submit",label = "Submit"),
                 textOutput(outputId = "d"),
                 textOutput(outputId = "error")
        ),
        tabPanel("Tables", fluid = TRUE,
                 textInput(inputId = "vmac", "VMAC ID", ""),
                 numericInput(inputId = "epoch", "Epoch", value=5),
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
            letter_conv <- c("fb"="Feedback", "LP"="LP Previsit","ty"="Thank You","previsit"="Previsit","pv"="Full Previsit","blood"="Blood Incidental","brain"="Brain Incidental","extra"="Extracardiac Incidental","echo"="Echo Incidental")
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
        
        if (epoch == 5) {
            results <- data.frame(
                "Epoch" = c("Enrollment", "3 Year", "5 Year", "7 Year"),
                dat
            )
        }
        
        if (epoch == 1){
            results <- data.frame(
                "Epoch" = c("Enrollment"),
                dat
            )
        }
        
        results <<- as.matrix(results)
        return(results)
        
    })
    output$ses <- renderDataTable({re2()})
    #output$exp1 <- renderText("Table has been Generated")
}

# Run the application 
shinyApp(ui = ui, server = server)
