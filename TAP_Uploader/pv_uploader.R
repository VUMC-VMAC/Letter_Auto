pv_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  
  loc_func <- function(X,loc_day) {
      loc <- c(paste0("Day ",X," of your visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."),
               paste0("Day ",X," of your visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A team member will meet you inside the hospital lobby."),
               paste0("Day ",X," of your visit will occur at your house."),
               paste0("Day ",X," of your visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."))
      loc_proxy <- c(paste0("Day ",X," of ",pronoun_poss," visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."),
                     paste0("Day ",X," of ",pronoun_poss," visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A team member will meet you inside the hospital lobby."),
                     paste0("Day ",X," of ",pronoun_poss," visit will occur at ",pronoun_poss," house."),
                     paste0("Day ",X," of ",pronoun_poss," visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."))
    location <- list("ptp" = loc[loc_day], "proxy" = loc_proxy[loc_day])
    return(location)
  }
  
  loc_func2 <- function(loc_day) {
    loc <- c(paste0("Day 2 and 3 of your visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."),
             paste0("Day 2 and 3 of your visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A team member will meet you inside the hospital lobby."),
             paste0("Day 2 and 3 of your visit will occur at your house."),
             paste0("Day 2 and 3 of your visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."))
    loc_proxy <- c(paste0("Day 2 and 3 of ",pronoun_poss," visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."),
                   paste0("Day 2 and 3 of ",pronoun_poss," visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A team member will meet you inside the hospital lobby."),
                   paste0("Day 2 and 3 of ",pronoun_poss," visit will occur at ",pronoun_poss," house."),
                   paste0("Day 2 and 3 of ",pronoun_poss," visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Please proceed to suite 302. If you require any assistance, please call 615-336-3388 and a team member will come down to escort you."))
    location <- list("ptp" = loc[loc_day], "proxy" = loc_proxy[loc_day])
    return(location)
  }
  
  is.even <- function(x) x %% 2 == 0
  
  print("Start")
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "0E16F65FB0A51C570781384D91AA1A78", conn, project = 137402)
  pdb_datas <- exportReports(pdb, 270432)
  pdb_datas[which(is.na(pdb_datas[,"proxy_diff_address"])),"proxy_diff_address"]<- "No"
 
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  pdb_data <- pdb_datas[ii,]
  
  e <- epoch
  Epoch_conv <- c("Baseline","Baseline","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("baseline","baseline","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e+1]; Epoc <<- Epoc_conv[e+1]
  epoch_conv <- c("base","base","18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch <<- epoch_conv[e+1]
  ep <- epoch_conv[e+1]
  
  i <- 1
  
  if (nchar(vmac)==1) {record <- paste0("0000",vmac)} else if (nchar(vmac)==2) {record <- paste0("000",vmac)} else if (nchar(vmac)==3) {record <- paste0("00",vmac)} else if (nchar(vmac)==4) {record <- paste0("0",vmac)} else {record <- vmac}
  input <- record
  
  first_name <<- pdb_data[,"preferred_name"] #change to preferred name
  if (is.na(first_name)) {first_name<<- pdb_data$first_name}
  last_name <<- pdb_data[,"last_name"]
  street_address <<- pdb_data[,"street_address"]
  city <<- pdb_data[,"city"]
  state <<- pdb_data[,"state"]
  zipp <<- pdb_data[,"zip"]
  salutation <<- as.character(pdb_data[,"salutation"])
  
  sex <<- as.character(pdb_data[,"sex"])
  gender <- ""
  if (sex=="Female") {gender<<- "women"} else {gender <<- "men"}
  if (sex=="Female") {gender_cap<<- "Women"} else {gender_cap <<- "Men"}
  proxy_sex <<- as.character(pdb_data[i,"proxy_sex"])
  pronoun_conv <- c("Female" = "she", "Male" = "he")
  pronoun_conv_obj <- c("Female" = "her", "Male" = "him")
  pronoun_conv_poss <- c("Female" = "her", "Male" = "his")
  pronoun_conv_poss_cap <- c("Female" = "Her", "Male" = "His")
  pronoun_conv_cap <- c("Female" = "She", "Male" = "He")
  pronoun <<- pronoun_conv[sex]
  pronoun_cap <<- pronoun_conv_cap[sex]
  pronoun_obj <<- pronoun_conv_obj[sex]
  pronoun_poss <<- pronoun_conv_poss[sex]
  pronoun_poss_cap <<- pronoun_conv_poss_cap[sex]
  
  proxy_pronoun <<- pronoun_conv[proxy_sex]
  proxy_first_name <<- pdb_data[,"proxy_first_name"]
  proxy_last_name <<- pdb_data[,"proxy_last_name"]
  proxy_salutation <<- as.character(pdb_data[,"proxy_salutation"])
  #proxy_sex <<- pdb_data[i,"proxy_sex"]
  
  if (pdb_data["proxy_diff_address"] == "Yes") {
    proxy_address <<- pdb_data[,"proxy_address"]
    proxy_city <<- pdb_data[,"proxy_city"]
    proxy_state <<- pdb_data[,"proxy_state"]
    proxy_zip <<- pdb_data[,"proxy_zip"]
  } else {
    proxy_address <<- pdb_data[,"street_address"]
    proxy_city <<- pdb_data[,"city"]
    proxy_state <<- pdb_data[,"state"]
    proxy_zip <<- pdb_data[,"zip"]
  }
  
  ############# Start The Divergence ############
  
  visit_type <- as.integer(pdb_data["visit_type"])
  print(visit_type)
  
  locat <- c("VMAC","Vanderbilt Hospital Valet","your home","VMAC")
  visit1_date <<- format(pdb_data[,"visit1_date"], "%A, %B %d, %Y")
  visit1_time <- as.character(pdb_data[,"visit1_time"])
  visit1_time <<- paste0(gsub(":00$","",visit1_time), "am")
  visit1_hours <<- as.character(pdb_data["visit1_hours"])
  loc_day1 <<- as.integer(pdb_data["visit1_location"]); X <<- 1
  location <- loc_func(X,loc_day1)
  location_day1 <<- location$ptp; location_day1_prox <<- location$proxy
  location_day1_prox_extra <<- ""; if (loc_day1 == 4) {location_day1_prox_extra <<- paste0("After completing the scheduled study assessments at the Vanderbilt Memory & Alzheimer\'s Center, ",pronoun," will travel to the Vanderbilt University Medical Center, 1210 Medical Center Drive, and arrive at the valet park station at the hospital entrance- valet parking is free. A VMAP team member will meet ",pronoun_obj," inside the hospital lobby.")}
  location_day1_extra <<- ""; if (loc_day1 == 4) {location_day1_extra <<- paste0("After completing the scheduled study assessments at the Vanderbilt Memory & Alzheimer\'s Center, you will travel to the Vanderbilt University Medical Center, 1210 Medical Center Drive, and arrive at the valet park station at the hospital entrance- valet parking is free. A VMAP team member will meet you inside the hospital lobby.")}
  #fu_time_7yr <- sub("0","",fu_time_7yr)
  visit2_date <<- format(pdb_data[,"visit2_date"], "%A, %B %d, %Y")
  visit2_time <- as.character(pdb_data[,"visit2_time"])
  visit2_time <<- paste0(gsub(":00$","",visit2_time),"am")
  visit2_hours <<- as.character(pdb_data[,"visit2_hours"])
  loc_day2 <<- as.integer(pdb_data["visit2_location"]); X <<- 2
  location <- loc_func(X,loc_day2)
  location_day2_3 <<- location$ptp; location_day2_3_prox <<- location$proxy
  day2 <<- paste0(visit2_date," at ",visit2_time)
  itin2 <<- "INSTRUCTIONS FOR 2DAY VISIT"
  
  path_in <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Previsit/pv2_ptp.docx")
  path_in_proxy <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Previsit/pv2_proxy.docx")
  
  location1 <- locat[loc_day1]
  location2 <- locat[loc_day2]
  
  if (loc_day1==4) {
    df1 <- data.frame(
      Day1 = c(paste0("Arrival at ",location1," at ",visit1_time),
               "Consent Process",
               "Paperwork Review",
               "Memory Testing",
               "Lunch",
               "Clinical Interview",
               "Transfer to Vanderbilt University Hospital",
               "Heart MRI",
               "Echo",
               "Brain MRI",
               paste("Study Wrap-up at ",visit1_time," plus ",visit1_hours)))
    ft1 <- flextable(df1) # Day 1 of 2-Day (1207 start)
  } else {
    df1 <- data.frame(
      Day1 = c(paste0("Arrival at ",location1," at ",visit1_time),
               "Consent Process",
               "Paperwork Review",
               "Memory Testing",
               "Lunch",
               "Clinical Interview",
               "Heart MRI",
               "Echo",
               "Brain MRI",
               paste("Study Wrap-up at ",visit1_time," plus ",visit1_hours)))
    ft1 <- flextable(df1) # Day 1 of 2-Day (Hospital start)
  }
  
  ft1 <- set_header_labels(ft1, Day1 = "Day 1 Itinerary")
  ft1 <- bg(ft1, bg="grey",part = "header")
  ft1 <- bg(ft1, i=c(2,4,6,8,10),bg="grey",part = "body")
  ft1 <- font(ft1,fontname = 'Arial')
  ft1 <- font(ft1,fontname = 'Arial',part = "header")
  ft1 <- fontsize(ft1,size = 13)
  ft1 <- fontsize(ft1,size = 13,part = "header")
  ft1 <- width(ft1,width = 4.5)
  ft1 <- theme_box(ft1)
  ft1 <- bold(ft1,bold = TRUE,part = "body")
  ft1 <<- align(ft1, align = "center", part="header")
  
  df2 <- data.frame(
    Day2 = c(paste0("Arrival at ",location2," at ",visit2_time),
             "Memory Testing",
             "Lunch",
             "Clinical Interview",
             "Heart MRI",
             "Echo",
             "Brain MRI",
             paste0("Study Wrap-up at ",visit2_time," plus ",visit2_hours)))
  ft2 <- flextable(df2) # Day 2 of 2-Day
  ft2 <- set_header_labels(ft2, Day2 = "Day 2 Itinerary")
  ft2 <- bg(ft2, bg="grey",part = "header")
  ft2 <- bg(ft2, i=c(2,4,6,8),bg="grey",part = "body")
  ft2 <- font(ft2,fontname = 'Arial')
  ft2 <- font(ft2,fontname = 'Arial',part = "header")
  ft2 <- fontsize(ft2,size = 13)
  ft2 <- fontsize(ft2,size = 13,part = "header")
  ft2 <- width(ft2,width = 4.5)
  ft2 <- theme_box(ft2)
  ft2 <- bold(ft2,bold = TRUE,part = "body")
  ft2 <<- align(ft2, align = "center", part="header")
  
  FT <<- list(ft1 = ft1, ft2 = ft2)
  
  if (visit_type==3) {
    visit3_date <<- format(pdb_data[,"visit3_date"], "%A, %B %d, %Y")
    visit3_time <- as.character(pdb_data[,"visit3_time"])
    visit3_time <<- paste0(gsub(":00$","",visit3_time),"am")
    visit3_hours <<- as.character(pdb_data["visit3_hours"])
    loc_day3 <<- as.integer(pdb_data["visit3_location"]); X <- 3
    location <- loc_func2(loc_day3)
    location_day2_3 <<- location$ptp; location_day2_3_prox <<- location$proxy
    day3 <<- paste0(visit3_date," at ",visit3_time)
    #add_day3 <<- paste0("The third day of your visit is scheduled for ",visit3_date," at ",visit3_time," and will last approximately ",visit3_hours," hours.")
    #add_day3_prox <<- paste0("The third day of ",pronoun_poss," visit is scheduled for ",visit3_date," at ",visit3_time," and will last approximately ",visit3_hours," hours.")
    #fu_time3_7yr <- sub("0","",fu_time3_7yr)
    
    # ADD PATHS HERE
    path_in <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Previsit/pv3_ptp.docx")
    path_in_proxy <- paste0("C:/Users/sweelyb/Documents/resources/Templates/Previsit/pv3_proxy.docx")
    
    location2 <- locat[loc_day2]
    location3 <- locat[loc_day3]
    
    if (loc_day1==4) {
      df1 <- data.frame(
        Day1 = c(paste0("Arrival at ",location1," at ",visit1_time),
                 "Consent Process",
                 "Paperwork Review",
                 "Memory Testing",
                 "Lunch",
                 "Clinical Interview",
                 "Transfer to Vanderbilt University Hospital",
                 "Heart MRI",
                 "Echo",
                 "Brain MRI",
                 paste("Study Wrap-up at ",visit1_time," plus ",visit1_hours)))
      ft1 <- flextable(df1) # Day 1 of 2-Day (1207 start)
    } else {
      df1 <- data.frame(
        Day1 = c(paste0("Arrival at ",location1," at ",visit1_time),
                 "Consent Process",
                 "Paperwork Review",
                 "Memory Testing",
                 "Break",
                 "Clinical Interview",
                 paste("Study Wrap-up at ",visit1_time," plus ",visit1_hours)))
      ft1 <- flextable(df1) # Day 1 of 2-Day (Hospital start)
    }
    
    ft1 <- set_header_labels(ft1, Day1 = "Day 1 Itinerary")
    ft1 <- bg(ft1, bg="grey",part = "header")
    ft1 <- bg(ft1, i=c(2,4,6),bg="grey",part = "body")
    ft1 <- font(ft1,fontname = 'Arial')
    ft1 <- font(ft1,fontname = 'Arial',part = "header")
    ft1 <- fontsize(ft1,size = 13)
    ft1 <- fontsize(ft1,size = 13,part = "header")
    ft1 <- width(ft1,width = 4.5)
    ft1 <- theme_box(ft1)
    ft1 <- bold(ft1,bold = TRUE,part = "body")
    ft1 <<- align(ft1, align = "center", part="header")
    
    df2 <- data.frame(
      Day2 = c(paste0("Arrival at ",location2," at ",visit2_time),
               "Echo",
               "Brain MRI",
               paste0("Study Wrap-up at ",visit2_time," plus ",visit2_hours)))
    ft2 <- flextable(df2) # Day 2 of 2-Day
    ft2 <- set_header_labels(ft2, Day2 = "Day 2 Itinerary")
    ft2 <- bg(ft2, bg="grey",part = "header")
    ft2 <- bg(ft2, i=c(2,4),bg="grey",part = "body")
    ft2 <- font(ft2,fontname = 'Arial')
    ft2 <- font(ft2,fontname = 'Arial',part = "header")
    ft2 <- fontsize(ft2,size = 13)
    ft2 <- fontsize(ft2,size = 13,part = "header")
    ft2 <- width(ft2,width = 4.5)
    ft2 <- theme_box(ft2)
    ft2 <- bold(ft2,bold = TRUE,part = "body")
    ft2 <<- align(ft2, align = "center", part="header")
    
    df3 <- data.frame(
      Day3 = c(paste0("Arrival at ",location3," at ",visit3_time),
               "Blood Work",
               "Change Clothes",
               "Lumbar Puncture",
               "Rest Period & Neurological Exam",
               "Breakfast, Change Clothes, & Physical Exam",
               paste0("Return to Valet; Depart at ",visit3_time," plus ",visit3_hours)))
    ft3 <- flextable(df3) # Day 2 of 2-Day
    ft3 <- set_header_labels(ft3, Day3 = "Day 3 Itinerary")
    ft3 <- bg(ft3, bg="grey",part = "header")
    ft3 <- bg(ft3, i=c(2,4,6),bg="grey",part = "body")
    ft3 <- font(ft3,fontname = 'Arial')
    ft3 <- font(ft3,fontname = 'Arial',part = "header")
    ft3 <- fontsize(ft3,size = 13)
    ft3 <- fontsize(ft3,size = 13,part = "header")
    ft3 <- width(ft3,width = 4.5)
    ft3 <- theme_box(ft3)
    ft3 <- bold(ft3,bold = TRUE,part = "body")
    ft3 <<- align(ft3, align = "center", part="header")
    
    FT <<- list(ft1 = ft1, ft2 = ft2, ft3 = ft3)
  }
  
  hotel_ <- pdb_data[,"visit_hotel"]
  if (is.na(hotel_)) {hotel_<-"No"}
  if (hotel_=="Yes") {hotel <<- "You will be residing at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. Your hotel confirmation is: XXXXXXXX."
  hotel_proxy <<- paste0("You will be residing at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. ",pronoun_poss_cap," hotel confirmation is: XXXXXXXX.")
  } else {hotel <<- "";hotel_proxy<<-""}
  
  transport <- pdb_data[,"visit_transport_needed"]
  if (is.na(transport)) {transport<-"No"}
  if (transport=="Yes") {t_need <<- "Your appointment will be held at the Vanderbilt University Medical Center. We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case you need to contact him."
  t_need_proxy <<- paste0(pronoun_poss_cap," appointment will be held at the Vanderbilt University Medical Center.  We will be providing transportation to and from ",pronoun_poss," visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case ",pronoun," needs to contact him.")
  } else {t_need <<- ""; t_need_proxy <<- ""}
  consent <<- paste0("Consent Statement. This document describes the VMAP Study. You completed this form when you originally came in for your eligibility visit, but we ask that you complete and sign this document again at each follow-up visit. Please read the form thoroughly before the appointment. We will ask you and your study partner, ",proxy_first_name,", to sign the consent form after we review it with you at the appointment. If your study partner does not plan to attend, we will mail them a copy to sign and return prior to your visit.")
  consent_prox <<- paste0("Consent Statement. This document describes the VMAP Study. You and ",first_name," already completed this form at the eligibility visit, but we ask that both of you sign this document again at each follow-up visit. Please read the form thoroughly. If you have questions, please let us know. If you will not be attending the visit, please return the signed copy prior to your visit.")
  
  if (pdb_data["proxy_diff_address"] == "Yes") {
    partner <<- paste0("Study Partner Packet. We have mailed a packet of material to your study partner, ",proxy_first_name,", which we ask that ",proxy_pronoun," complete prior to your visit.")
    partner_prox <<- paste0("Study Partner Packet. This packet is for you. Please complete these questionnaires prior to ",pronoun_poss," visit. If you will not be attending, return the signed consent document and completed questionnaires in the included prepaid envelope.")
  } else {
    partner <<- paste0("Study Partner Packet. We have included a packet of material for your study partner, ",proxy_first_name,", which we ask that ",proxy_pronoun," complete prior to your visit.")
    partner_prox <<- paste0("Study Partner Packet. This packet is for you. Please complete these questionnaires prior to ",pronoun_poss," visit. If you will not be attending, return the signed consent document and completed questionnaires in the included prepaid envelope.")
  }
  
  proxy_require <- pdb_data[,"visit_proxy_require"]
  if (is.na(proxy_require)) {proxy_require<-"No"}
  if (proxy_require == "Yes") {
    p_req <<- "Your presence is required for all study visits. Feel free to bring a book or other form of entertainment to keep you occupied during your waiting period."
    p_imp <<- paste0("In addition to your questionnaires, please complete the following forms & questionnaires on behalf of ",first_name,": The Medical Authorization of Release, Health History Questionnaire, Minnesota Leisure, Quick Food Scan, CHAMPS, and Pittsburgh Sleep Quality Index.")
    #histor <<- ""
  } else {
    p_req <<- "Your presence is not required however you are welcome to attend."
    p_imp <<- ""
    #histor <<- paste0("Medical History Forms & Questionnaires. Prior to your appointment, please complete ALL questionnaires. Please be sure to bring your medications to the visit so our team can review them with you.")
  }
  
  field <- "visit_letter"
  field_proxy <- "visit_letter_proxy"
    
  
  
  temp <- "C:/Users/sweelyb/Documents/resources/Output/previsit_temp.docx"
  temp_proxy <- "C:/Users/sweelyb/Documents/resources/Output/previsit_temp_prox.docx"
  
  body_add_flextables(path_in,temp, FT)
  body_add_flextables(path_in_proxy,temp_proxy, FT)
  
  output <- paste0("C:/Users/sweelyb/Documents/resources/Output/TAP_",input,"_ptp_letter.docx")
  renderInlineCode(temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = field, event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  
  if (is.na(proxy_first_name)==FALSE) {
    output_proxy <- paste0("C:/Users/sweelyb/Documents/resources/Output/TAP_",input,"_proxy_letter.docx")
    renderInlineCode(temp_proxy, output_proxy)
    
    importFiles(rcon = pdb, file = output_proxy, record = record, field = field_proxy, event = pdb_data[,"redcap_event_name"],
                overwrite = TRUE, repeat_instance = 1)
  }
}