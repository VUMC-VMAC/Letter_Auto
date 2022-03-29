vmac_fu_uploader <- function(vmac_id) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(DiagrammeR)
  library(reticulate)
  library(RCurl)
  
  dir(path = paste0(getwd(), "/Letter_Auto/resources/Templates/VMAC Registry"), pattern = ".docx")
  
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  
  token <- "31F3F4459CDE37C4327B75A3A07236C3"
  url <- "https://redcap.vanderbilt.edu/api/"
  formData <- list("token"=token,
                   content='record',
                   action='export',
                   format='json',
                   type='flat',
                   csvDelimiter='',
                   'records[0]'=record,
                   'fields[0]'='vmac_id',
                   'fields[1]'='first_name',
                   'fields[2]'='last_name',
                   'fields[3]'='salutation',
                   'fields[4]'='preferred_name',
                   'fields[5]'='sex',
                   #'forms[0]'='vmac_participant_questionnaire_static',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  vmac <- httr::content(response); vmac1 <- vmac[[1]]
  
  formData <- list("token"=token,
                   content='record',
                   action='export',
                   format='json',
                   type='flat',
                   csvDelimiter='',
                   'records[0]'=record,
                   #'fields[0]'='vmac_id',
                   'fields[0]'='street_address',
                   'fields[1]'='city',
                   'fields[2]'='state',
                   'fields[3]'='zip',
                   #'forms[0]'='vmac_participant_questionnaire_static',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  vmac <- httr::content(response); vmac2 <- vmac[[1]]
  
  formData <- list("token"=token,
                   content='record',
                   action='export',
                   format='json',
                   type='flat',
                   csvDelimiter='',
                   'records[0]'=record,
                   #'fields[0]'='vmac_id',
                   'fields[0]'='tp_user',
                   'fields[1]'='tp_date',
                   #'forms[0]'='vmac_participant_questionnaire_static',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  vmac <- httr::content(response); tp <- vmac[[1]]
  
  
  #try(response <- httr::POST(url, body = formData, encode = "form"), silent = TRUE)
  if (exists("response")==FALSE) {print("No Updates")} else {
    #vmacs <<- httr::content(response)
    #df <- data.frame(vmacs[[1]])
    #for (i in (2:length(vmacs))) {
    #  df <- rbind(df,vmacs[[i]])
    #}
    
    # ID Selector
    #dff <<-  df["vmac_id"][[1]]
    #try(indd <- grep(vmac_id,dff),silent = TRUE)
    #vmac <<- vmacs[indd]
    #vmac1 <<- vmac[[1]]
    #vmac2 <<- vmac[[2]]
    #tp <- vmac[[]]
    
    
    first_name <<- vmac1$first_name
    last_name <<- vmac1$last_name
    address <<- vmac2$street_address
    address_city <<- vmac2$city
    address_city <<- gsub(" $","",address_city)
    address_state <<- vmac2$state
    address_zip <<- vmac2$zip
    sal <- as.character(vmac1$salutation)
    sal_conv <- c("1"="Mr.","2"="Mrs.","3"="Ms.","4"="Dr.","5"="Rev.")
    salutation <<- sal_conv[sal]
    screen_p <- tp$tp_user
    
    
    screen_conv <- c("98"="Tineciaa Harris","11"="Katie Gifford","59"="Jessica Steele","3"="Sydney Wilhoite","15"="Raymond Romano","6"="Paige Crepezzi","21"="Natalie Pettirossi","16"="Chloe Motley","20"="Dominic Roby","22"="Sope Adeleye", "74"="Maddy Berkowitz-Cerasano", "89"="Sameeksha Malhotra","64"="Natalie Givens", "30"="Marilyn Steinbach", "93" ="Shelbie Wenner", "62"="Jordan Rahm")
    initial_conv <- c("98"="TH","11"="KG","59"="JS","3"="SW","15"="RR","6"="PC","21"="NP","16"="CM","20"="DR","22"="SA","74"="MBC","89"="SHM","64"="NRG","30"="MS","93"="SW","62"="JR")
    
    
    screen_person <<- screen_conv[screen_p]
    initial <<- initial_conv[screen_p]
    #letter_sent <- vmac[["vmac_followup_letter_mailing"]]
    output <- paste0("C:/Users/sweelyb/Documents/Output/VMAC_registry/VMAC_",vmac1$vmac_id,"_",initial,".docx")
    path_in <- paste0("C:/Users/sweelyb/Documents/Letter_Auto/resources/Templates/VMAC Registry/VMAC_template.docx")
    renderInlineCode(path_in, output)
    #importFiles(rcon = vmac_database, file = output, record = vmac_id, field = "vmac_letter", event='initial_vmac_call_arm_1',
    #            overwrite = TRUE, repeat_instance = 1)
    cmmd <- paste0("python C:/Users/sweelyb/Documents/Letter_Auto/resources/fu_uploader.py -v ",record," -f \"",output,"\"")
    system(cmmd)
    #}
  }
}