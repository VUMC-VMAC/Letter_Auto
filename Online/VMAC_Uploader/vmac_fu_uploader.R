vmac_fu_uploader <- function(vmac_id) {
  
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
                   report_id='243399',
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   returnFormat='json'
  )
  #response <- httr::POST(url, body = formData, encode = "form")
  try(response <- httr::POST(url, body = formData, encode = "form"), silent = TRUE)
  if (exists("response")==FALSE) {print("No Updates")} else {
    vmacs <<- httr::content(response)
    df <- data.frame(vmacs[[1]])
    for (i in (2:length(vmacs))) {
      df <- rbind(df,vmacs[[i]])
    }
    
    # ID Selector
    dff <<-  df["vmac_id"][[1]]
    try(indd <- grep(vmac_id,dff),silent = TRUE)
    vmac <<- vmacs[[indd]]
    
    
    first_name <<- vmac[["first_name"]]
    last_name <<- vmac[["last_name"]]
    address <<- vmac[["address"]]
    address_city <<- vmac[["address_city"]]
    address_city <<- gsub(" $","",address_city)
    address_state <<- vmac[["address_state"]]
    address_zip <<- vmac[["address_zip"]]
    sal <- as.character(vmac[["salutation"]])
    sal_conv <- c("1"="Mr.","2"="Mrs.","3"="Ms.","4"="Dr.","5"="Rev.")
    salutation <<- sal_conv[sal]
    screen_p <- vmac[["screen_person"]]
    screen_conv <- c("93"="Nicole Boog","98"="Natalie Pettirossi","92"="Mekenzie Meadows","96"="Chloe Motley","91"="Dominic Roby","94"="Sydney Wilhoite","81"="Sope Adeleye", "33"="Maddy Berkowitz-Cerasano", "61"="Sameeksha Malhotra","64"="Natalie Givens", "32"="Marilyn Steinbach", "62" ="Shelbie Wenner", "31"="Jordan Rahm","63"="Hannah Gavins","35"="Malek Jacobs","3"="Samantha Brown","34"="Emily Bradford")
    initial_conv <- c("93"="NB","98"="NP","92"="MM","96"="CM","91"="DR","94"="SW","81"="SA","33"="MBC","61"="SHM","64"="NRG","32"="MS","62"="SW","31"="JR","63"="HG","35"="MJ","30"="SB","34"="EB")
    screen_person <<- screen_conv[screen_p]
    initial <<- initial_conv[screen_p]
    #letter_sent <- vmac[["vmac_followup_letter_mailing"]]
    output <- paste0("/app/VMAC_",vmac[["vmac_id"]],"_",initial,".docx")
    path_in <- paste0("/srv/shiny-server/resources/Templates/VMAC Registry/VMAC_template.docx")
    renderInlineCode(path_in, output)
    #importFiles(rcon = vmac_database, file = output, record = vmac_id, field = "vmac_letter", event='initial_vmac_call_arm_1',
    #            overwrite = TRUE, repeat_instance = 1)
    #cmmd <- paste0("python /srv/shiny-server/resources/fu_uploader.py -v ",vmac[["vmac_id"]]," -f \"",output,"\"")
    #system(cmmd)
    formData <- list("token"=token,content='file',action='import',record=vmac[["vmac_id"]],
                     field='vmac_letter',event='initial_vmac_call_arm_1',
                     returnFormat='json',file=httr::upload_file(output))
    response <- httr::POST(url,body=formData,encode="multipart")
    result <- httr::content(response)
    print(result)
    #}
  }
}
