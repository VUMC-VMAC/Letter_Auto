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

source("~/MAP Uploader/MAP_uploader.R")
source("~/MAP Uploader/ty_uploader.R")
source("~/MAP Uploader/fb_uploader.R")
source("~/MAP Uploader/LP_uploader.R")

library(shiny)
library(flextable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Participant Letter Generator"),
    selectInput(inputId = "n", "MAP ID", choices=c("002","010","011","013","014","015","017","020","021","024","026","027","028","030","031","032","033","034","035","036","038","043","044","045","046","048","049","050","053","054","055","057","058","060","062","063","064","065","067","068","071","072","073","074","075","076","078","079","080","083","084","086","087","088","089","090","091","092","095","097","099","100","102","103","104","105","107","111","113","114","116","117","118","120","122","124","125","126","127","130","131","133","134","135","136","138","139","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","160","161","162","163","164","165","166","168","169","170","171","172","173","174","175","176","178","179","180","182","183","185","186","187","188","189","190","191","192","194","195","196","200","202","203","204","205","206","207","208","209","210","211","215","216","218","219","222","225","228","229","230","231","232","23","235","236","237","239","241","242","243","244","245","246","248","249","251","252","253","254","255","256","257","259","260","262","263","264","266","267","268","270","271","272","273","274","275","276","277","278","279","280","281","283","284","285","286","287","289","292","295","296","297","298","299","301","302","303","304","306","307","308","309","310","311","312","313","314","316","317","319","322","323","324","325","327","328","329","330","331","333","334","335","337"),
                multiple = TRUE),
    numericInput(inputId = "d", "Epoch", value=5),
    selectInput(inputId = "l", "Type of Letter", choices = c("Feedback Letter" = "fb", "LP Previsit Letter" = "LP", "MAP Previsit Letter"="MAP","Thank You"="ty")),
    #submitButton("Generate Document", icon("refresh")),
    
    #checkboxInput(inputId = "check", label = "Make letter for all"),
    
    actionButton(inputId = "submit",label = "Submit"),
    textOutput(outputId = "d")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #re <- reactive({
    re <- eventReactive( input$submit, {
            letter <- input$l
            epoch <- input$d
            map <- input$n
            #check <- input$check
            
            #if (check) {
                # Run uploader scripts that loop through each ptp
            #}
            
            command <- paste0(letter,"_uploader(",epoch,",",map,")")
            eval(parse(text = command))
            letter_conv <- c("fb"="Feedback", "LP"="LP Previsit", "MAP"="MAP Previsit","ty"="Thank You")
            lett <- letter_conv[letter]
            confirmation <<- paste0(lett," Letter for MAP ID: ",input$n," has been generated!")
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
