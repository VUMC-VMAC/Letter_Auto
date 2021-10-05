LP_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "CBF02E285BFC1874F0EAF11D3F4E2842", conn, project = 23166)
  pdb_data <- exportReports(pdb, 252802)
  
#if (exists("pdb_data")==FALSE) {print("No Updates")} else {
  #for (i in 1:nrow(pdb_data)) {
  i <- which(pdb_data["vmac_id"]==as.integer(vmac)) #need to find i for map id
  e <- epoch
  ep_conv <- c("elig","18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  ep <- ep_conv[e]
  
  map_id <- as.character(pdb_data[i,"map_id"])
  if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  vmac_id <- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  
  first_name <<- pdb_data[i,"first_name"] #change to preferred name
  last_name <<- pdb_data[i, "last_name"]
  street_address <<- pdb_data[i, "street_address"]
  city <<- pdb_data[i, "city"]
  state <<- pdb_data[i, "state"]
  zipp <<- pdb_data[i, "zip"]
  salutation <<- as.character(pdb_data[i,"salutation"])
  lp_date_7yr <<- format(as.Date(pdb_data[i,"lp_date_7yr"]), "%A, %B %d, %Y")
  lp_time_7yr <<- as.character(pdb_data[i,"lp_time_7yr"])
  lp_time_7yr <<- paste0(lp_time_7yr,"am")
  
  df <- data.frame(
    Day1 = c(paste0("Arrival at Vanderbilt Hospital Valet at ",lp_time_7yr),
             "Transition to CRC/Change Clothes",
             "Lumbar Puncture",
             "Rest Period",
             "Breakfast, Change Clothes",
             # "[Add Additional Study Components if relevant]",
             paste0("Return to Valet, Depart at 3 hours past ",lp_time_7yr)))
  ft <- flextable(df) # 1-Day
  ft <- set_header_labels(ft, Day1 = "Lumbar Puncture Itinerary")
  ft <- bg(ft, bg="grey",part = "header")
  ft <- bg(ft, i=c(2,4,6),bg="grey",part = "body")
  ft <- fontsize(ft,size = 13)
  ft <- fontsize(ft,size = 13,part = "header")
  ft <- width(ft,width = 4.5)
  ft <- theme_box(ft)
  ft <- bold(ft,bold = TRUE,part = "body")
  ft <- border_inner(ft,border = fp_border(width = 0),part = "body")
  ft <<- align(ft, align = "center", part="header")
  FT <<- list(ft = ft)
  
  fu_transport_7yr <- pdb_data[i,"fu_transport_7yr"]
  if (is.na(fu_transport_7yr)) {fu_transport_7yr<-"No"}
  if (fu_transport_7yr == "No") {
    directions <<- "Your appointment will be held at the Vanderbilt University Medical Center.  Please use valet to park your car at 1210 Medical Center Drive (noted with a star on the enclosed map).  Valet parking is free.  A member of our team will meet you at the valet to take you to the Clinical Research Center for your visit.  You will be asked to wear a mask throughout your visit; if you do not have a mask, one will be provided."
  } else {
    directions <<- "Your appointment will be held at the Vanderbilt University Medical Center.  We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is (615) 604-1502 in case you need to contact him."
  }
  
  output <- paste0("C:/Users/sweelyb/Documents/resources/Output/MAP_",ep,"_",input,"_LP_letter.docx")
  path_in <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Lumbar Puncture/LP_template.docx")
  temp <- paste0("~/resources/LP_temp.docx")
  
  body_add_flextables(path_in,temp, FT)
  renderInlineCode(temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "lp_letter_7yr",
              overwrite = TRUE, repeat_instance = 1)
  #}
#}
}