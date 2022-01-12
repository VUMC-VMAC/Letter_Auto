LP_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 283621)
  
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  pdb_data <- pdb_datas[ii,]
  
  e <- epoch
  ep_conv <- c("base","18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  ep <- ep_conv[e]
  
  i <- 1
  
  #map_id <- as.character(pdb_data[i,"map_id"])
  #if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  #vmac_id <- as.character(pdb_data[i,"vmac_id"])
  #if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  
  if (nchar(vmac)==1) {record <- paste0("0000",vmac)} else if (nchar(vmac)==2) {record <- paste0("000",vmac)} else if (nchar(vmac)==3) {record <- paste0("00",vmac)} else if (nchar(vmac)==4) {record <- paste0("0",vmac)} else {record <- vmac}
  input <- record
  
  first_name <<- pdb_data[i,"first_name"] #change to preferred name
  last_name <<- pdb_data[i, "last_name"]
  street_address <<- pdb_data[i, "street_address"]
  city <<- pdb_data[i, "city"]
  state <<- pdb_data[i, "state"]
  zipp <<- pdb_data[i, "zip"]
  salutation <<- as.character(pdb_data[i,"salutation"])
  lp_date <<- format(as.Date(pdb_data[i,"lp_date"]), "%A, %B %d, %Y")
  lp_time <<- as.character(pdb_data[i,"lp_time"])
  lp_time <<- paste0(lp_time,"am")
  
  df <- data.frame(
    Day1 = c(paste0("Arrival at Vanderbilt Hospital Valet at ",lp_time),
             "Transition to CRC/Change Clothes",
             "Lumbar Puncture",
             "Rest Period",
             "Breakfast, Change Clothes",
             # "[Add Additional Study Components if relevant]",
             paste0("Return to Valet, Depart at 3 hours past ",lp_time)))
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
  
  fu_transport <- pdb_data[i,"visit_transport_needed"]
  if (is.na(fu_transport)) {fu_transport<-"No"}
  if (fu_transport == "No") {
    directions <<- "Your appointment will be held at the Vanderbilt University Medical Center.  Please use valet to park your car at 1210 Medical Center Drive (noted with a star on the enclosed map).  Valet parking is free.  A member of our team will meet you at the valet to take you to the Clinical Research Center for your visit."
  } else {
    directions <<- "Your appointment will be held at the Vanderbilt University Medical Center.  We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is (615) 604-1502 in case you need to contact him."
  }
  
  output <- paste0("/app/Output/TAP_",ep,"_",input,"_LP_letter.docx")
  path_in <- paste0("/srv/shiny-server/resources/Templates/Lumbar Puncture/TAP_LP_template.docx")
  temp <- paste0("/app/Output/LP_temp.docx")
  
  body_add_flextables(path_in,temp, FT)
  renderInlineCode(temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "lp_letter", event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  #}
#}
}