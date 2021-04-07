#!/usr/bin/env Rscript

library(redcapAPI)
library(WordR)
library(officer)
library(Hmisc)
library(tidyverse)
library(DiagrammeR)
library(reticulate)
library(RCurl)

dir(path = paste0(getwd(), "/resources/Templates/VMAC Registry"), pattern = ".docx")

token <- "B07286F2BCFC9B49C157FD44A62F3320"
url <- "https://redcap.vanderbilt.edu/api/"
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='242473',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
#response <- httr::POST(url, body = formData, encode = "form")
try(response <- httr::POST(url, body = formData, encode = "form"), silent = TRUE)
if (exists("response")==FALSE) {print("No Updates")} else {
  vmac <- httr::content(response)
  
  #for (i in 1:3) {
  i<-1
    vmac_id <- vmac[[i]][["vmac_id"]]
    first_name <- vmac[[i]][["first_name"]]
    last_name <- vmac[[i]][["last_name"]]
    address <- vmac[[i]][["address"]]
    address_city <- vmac[[i]][["address_city"]]
    address_city <- gsub(" $","",address_city)
    address_state <- vmac[[i]][["address_state"]]
    address_zip <- vmac[[i]][["address_zip"]]
    sal <- as.character(vmac[[i]][["salutation"]])
    sal_conv <- c("1"="Mr.","2"="Mrs.","3"="Ms.","4"="Dr.","5"="Rev.")
    salutation <- sal_conv[sal]
    screen_p <- vmac[[i]][["screen_person"]]
    screen_conv <- c("81"="Sope Adeleye", "33"="Maddy Berkowitz-Cerasano", "61"="Sameeksha Malhotra","64"="Natalie Givens", "32"="Marilyn Steinbach", "62" ="Shelbie Wenner", "31"="Jordan Rahm","63"="Hannah Gavins","35"="Malek Jacobs","3"="Samantha Brown")
    initial_conv <- c("81"="SA","33"="MBC","61"="SHM","64"="NRG","32"="MS","62"="SW","31"="JR","63"="HG","35"="MJ","30"="SB")
    screen_person <- screen_conv[screen_p]
    initial <- initial_conv[screen_p]
    letter_sent <- vmac[[i]][["vmac_followup_letter_mailing"]]
    output <- paste0("C:/Users/sweelyb/Documents/resources/Output/VMAC_registry/VMAC_",vmac_id,"_",initial,".docx")
    path_in <- paste0("C:/Users/sweelyb/Documents/resources/Templates/VMAC Registry/VMAC_template.docx")
    renderInlineCode(path_in, output)
    #importFiles(rcon = vmac_database, file = output, record = vmac_id, field = "vmac_letter", event='initial_vmac_call_arm_1',
    #            overwrite = TRUE, repeat_instance = 1)
    cmmd <- paste0("python C:/Users/sweelyb/Documents/resources/fu_uploader.py -v ",vmac_id," -f \"",output,"\"")
    system(cmmd)
  #}
}