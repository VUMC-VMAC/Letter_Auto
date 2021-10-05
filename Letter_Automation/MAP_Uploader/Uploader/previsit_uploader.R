previsit_uploader <- function(epoch,vmac) {
  
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  
  loc_func <- function(X,loc_day) {
      loc <- c(paste0("Day ",X," of your visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to check temperature."),
               paste0("Day ",X," of your visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A VMAP team member will meet you inside the hospital lobby."),
               paste0("Day ",X," of your visit will occur at your house."),
               paste0("Day ",X," of your visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to check temperature."))
      loc_proxy <- c(paste0("Day ",X," of ",pronoun_poss," visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to check temperature."),
                     paste0("Day ",X," of ",pronoun_poss," visit will be held at the Vanderbilt University Medical Center, located on 1210 Medical Center Drive (noted with stars on the enclosed maps). Please valet park at the hospital entrance on 1210 Medical Center Drive - valet parking is free. A VMAP team member will meet you inside the hospital lobby."),
                     paste0("Day ",X," of ",pronoun_poss," visit will occur at ",pronoun_poss," house."),
                     paste0("Day ",X," of ",pronoun_poss," visit will start at the Vanderbilt Memory and Alzheimer\'s Center and end at the Vanderbilt University Medical Center. Arrive at Vanderbilt Memory & Alzheimer\'s Center, 1207 17th Avenue S., Suite 302, park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to check temperature."))
    location <- list("ptp" = loc[loc_day], "proxy" = loc_proxy[loc_day])
    return(location)
  }
  
  print("Start")
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "7E1DC8A562246EC4F7043579B863706C", conn, project = 136242)
  pdb_data <- exportReports(pdb, 267462)
  #try(pdb_data <- exportReports(pdb, 252698), silent = TRUE)
  #if (exists("pdb_data")==FALSE) {print("No Updates")} else {
  pdb_data[which(is.na(pdb_data[,"proxy_diff_address"])),"proxy_diff_address"]<- "No"
  
  events <- c("eligibility_arm_1","enrollmentbaseline_arm_1","18month_followup_arm_1","3year_followup_arm_1","5year_followup_arm_1","7year_followup_arm_1")
  pdb_datas <- pdb_data[which(pdb_data[,"redcap_event_name"]== events[epoch+1]),]
  
  ii <- which(pdb_datas["vmac_id"]==as.integer(vmac))
  pdb_data <- pdb_datas[ii,]
  
  e <- epoch
  Epoch_conv <- c("Eligibility","Enrollment","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("eligibility","enrollment","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e+1]; Epoc <<- Epoc_conv[e+1]
  epoch_conv <- c("elig","enroll","18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch <<- epoch_conv[e+1]
  ep <- epoch_conv[e+1]
  
  i <- 1
  #map_id <- as.character(pdb_data[i,"map_id"])
  #if (nchar(map_id)==1) {input <- paste0("00",map_id)} else if (nchar(map_id)==2) {input <- paste0("0",map_id)} else {input <- map_id}
  vmac_id <- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record <- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record <- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record <- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record <- paste0("0",vmac_id)} else {record <- vmac_id}
  input <- record
  
  first_name <<- pdb_data[i,"preferred_name"] #change to preferred name
  if (is.na(first_name)) {first_name <<- pdb_data[i,"first_name"]}
  last_name <<- pdb_data[i, "last_name"]
  street_address <<- pdb_data[i, "street_address"]
  city <<- pdb_data[i, "city"]
  state <<- pdb_data[i, "state"]
  zipp <<- pdb_data[i, "zip"]
  salutation <<- as.character(pdb_data[i,"salutation"])
  
  sex <<- as.character(pdb_data[i, "sex"])
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
  proxy_first_name <<- pdb_data[i,"proxy_first_name"]
  proxy_last_name <<- pdb_data[i,"proxy_last_name"]
  proxy_salutation <<- as.character(pdb_data[i,"proxy_salutation"])
  #proxy_sex <<- pdb_data[i,"proxy_sex"]
  
  if (pdb_data[i,"proxy_diff_address"] == "Yes") {
    proxy_address <<- pdb_data[i, "proxy_address"]
    proxy_city <<- pdb_data[i, "proxy_city"]
    proxy_state <<- pdb_data[i, "proxy_state"]
    proxy_zip <<- pdb_data[i, "proxy_zip"]
  } else {
    proxy_address <<- pdb_data[i, "street_address"]
    proxy_city <<- pdb_data[i, "city"]
    proxy_state <<- pdb_data[i, "state"]
    proxy_zip <<- pdb_data[i, "zip"]
  }
  
  ############# Start The Divergence of Epochs ############
  
  if (e > 0) {
    
    ############# EPOCH 5 Starts HERE ###################
    
    visit_type <- as.integer(pdb_data[i,"visit_type"])
    
    add_day3<<-""
    add_day3_prox<<-""
    day3 <<- ""
    add_day2<<-""
    add_day2_prox<<-""
    day2 <<- ""
    location_day2<<-""
    location_day2_prox<<-""
    location_day3<<-""
    location_day3_prox<<-""
    visit1_date <<- format(as.Date(pdb_data[i, "visit1_date"]), "%A, %B %d, %Y")
    visit1_time <- as.character(pdb_data[i,"visit1_time"])
    visit1_time <<- paste0(gsub(":00$","",visit1_time), "am")
    visit1_hours <<- as.character(pdb_data[i,"visit1_hours"])
    loc_day1 <<- as.integer(pdb_data[i,"visit1_location"]); X <<- 1
    location <- loc_func(X,loc_day1)
    location_day1 <<- location$ptp; location_day1_prox <<- location$proxy
    location_day1_prox_extra <<- ""; if (loc_day1 == 4) {location_day1_prox_extra <<- paste0("After completing the scheduled study assessments at the Vanderbilt Memory & Alzheimer\'s Center, ",pronoun," will travel to the Vanderbilt University Medical Center, 1210 Medical Center Drive, and arrive at the valet park station at the hospital entrance- valet parking is free. A VMAP team member will meet ",pronoun_obj," inside the hospital lobby.")}
    location_day1_extra <<- ""; if (loc_day1 == 4) {location_day1_extra <<- paste0("After completing the scheduled study assessments at the Vanderbilt Memory & Alzheimer\'s Center, you will travel to the Vanderbilt University Medical Center, 1210 Medical Center Drive, and arrive at the valet park station at the hospital entrance- valet parking is free. A VMAP team member will meet you inside the hospital lobby.")}
    #fu_time_7yr <- sub("0","",fu_time_7yr)
    if (visit_type==1 | visit_type==3 | visit_type==5 | visit_type==6){
      visit2_date <<- format(as.Date(pdb_data[i, "visit2_date"]), "%A, %B %d, %Y")
      visit2_time <- as.character(pdb_data[i,"visit2_time"])
      visit2_time <<- paste0(gsub(":00$","",visit2_time),"am")
      visit2_hours <<- as.character(pdb_data[i,"visit2_hours"])
      loc_day2 <<- as.integer(pdb_data[i,"visit2_location"]); X <<- 2
      location <- loc_func(X,loc_day2)
      location_day2 <<- location$ptp; location_day2_prox <<- location$proxy
      day2 <<- paste0("and ",visit2_date," at ",visit2_time)
      add_day2 <<- paste0("The second day of your visit is scheduled for ",visit2_date," at ",visit2_time," and will last approximately ",visit2_hours," hours.")
      add_day2_prox <<- paste0("The second day of ",pronoun_poss," visit is scheduled for ",visit2_date," at ",visit2_time," and will last approximately ",visit2_hours," hours.")
      itin2 <<- "INSTRUCTIONS FOR 2DAY VISIT"
      #fu_time2_7yr <- sub("0","",fu_time2_7yr)
      if (visit_type==3) {
        visit3_date <<- format(as.Date(pdb_data[i, "visit3_date"]), "%A, %B %d, %Y")
        visit3_time <- as.character(pdb_data[i,"visit3_time"])
        visit3_time <<- paste0(gsub(":00$","",visit3_time),"am")
        visit3_hours <<- as.character(pdb_data[i,"visit3_hours"])
        loc_day3 <<- as.integer(pdb_data[i,"visit3_location"]); X <- 3
        location <- loc_func(X,loc_day3)
        location_day3 <<- location$ptp; location_day3_prox <<- location$proxy
        day3 <<- paste0("and ",visit3_date," at ",visit3_time)
        add_day3 <<- paste0("The third day of your visit is scheduled for ",visit3_date," at ",visit3_time," and will last approximately ",visit3_hours," hours.")
        add_day3_prox <<- paste0("The third day of ",pronoun_poss," visit is scheduled for ",visit3_date," at ",visit3_time," and will last approximately ",visit3_hours," hours.")
        #fu_time3_7yr <- sub("0","",fu_time3_7yr)
      }
      if (visit_type==4) {
        visit4_date <<- format(as.Date(pdb_data[i, "visit4_date"]), "%A, %B %d, %Y")
        visit4_time <- as.character(pdb_data[i,"visit4_time"])
        visit4_time <<- paste0(gsub(":00$","",visit4_time),"am")
        visit4_hours <<- as.character(pdb_data[i,"visit4_hours"])
        location_day4 <<- as.integer(pdb_data[i,"visit4_location"]); X <- 4
        add_day4 <<- paste0("The third day of your visit is scheduled for ",visit4_date," at ",visit4_time," and will last approximately ",visit4_hours)
      }
    }
    cdrq<<-"";cdrq_prox<<-"";envel<<-""
    if (visit_type==7) {
      cdrq <<- "Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you. 
    The copy labeled \'KEEP\' is for you to keep for your records. 
    "
      cdrq_prox <<- paste0("Because ",pronoun," will only be completing a phone interview and questionnaires, most of this consent document do not apply. There is a note on the first page of the document stating that ",pronoun," will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you. 
    The copy labeled \'KEEP\' is for you to keep for your records. 
    ")
      envel <<- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
    }
    if (visit_type==8) {
      cdrq <<- "Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you.  
    The copy labeled \'KEEP\' is for you to keep for your records. 
    "
      cdrq_prox <<- paste0("Because ",pronoun," will only be completing a phone interview and questionnaires, most of this consent document do not apply. There is a note on the first page of the document stating that ",pronoun," will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you. 
    The copy labeled \'KEEP\' is for you to keep for your records. 
    ")
      envel <<- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
    }
    if (visit_type==9) {
      cdrq <<- "Because you will only be completing a phone interview and questionnaires, most of this consent document does not apply to you. There is a note on the first page of the document stating that you will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply to you. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you.  
    The copy labeled \'KEEP\' is for you to keep for your records. 
    "
      cdrq_prox <<- paste0("Because ",pronoun," will only be completing a phone interview and questionnaires, most of this consent document do not apply. There is a note on the first page of the document stating that ",pronoun," will be completing the questionnaires and interview by phone only. 
    We have already marked \"No\" for each optional piece in the document because these items do not apply. 
    We have enclosed two copies of the consent form labeled \'RETURN\' and \'KEEP\' on the top of the first page. 
    We will call you to ensure you have received the consent form and have had a chance to review it and ask questions before signing and returning it. Please do not return this form until we have had a chance to review it with you. 
    The copy labeled \'KEEP\' is for you to keep for your records. 
    ")
      envel <<- "Stamped/Addressed Envelope. We have included a stamped and pre-addressed envelope for you to mail back your paperwork."
    }
    
    fu_proxy_7yr <- pdb_data[i,"visit_proxy_require"]
    if (is.na(fu_proxy_7yr)) {fu_proxy_7yr<-"No"}
    if (fu_proxy_7yr == "Yes") {
      p_req <<- "Your presence is required for all study visits. Feel free to bring a book or other form of entertainment to keep you occupied during your waiting period."
      p_imp <<- paste0("In addition to your questionnaires, please complete the following forms & questionnaires on behalf of ",first_name,": The Medical Authorization of Release, Health History Questionnaire, Minnesota Leisure, Quick Food Scan, CHAMPS, and Pittsburgh Sleep Quality Index.")
      histor <<- ""
    } else {
      p_req <<- "Your presence is not required however you are welcome to attend."
      p_imp <<- ""
      histor <<- paste0("Medical History Forms & Questionnaires. Prior to your appointment, please complete ALL questionnaires. Please be sure to bring your medications to the visit so our team can review them with you.")
    }
    
    fu_hotel_7yr <- pdb_data[i,"visit_hotel"]
    if (is.na(fu_hotel_7yr)) {fu_hotel_7yr<-"No"}
    if (fu_hotel_7yr=="Yes") {hotel <<- "You will be residing at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. Your hotel confirmation is: XXXXXXXX."
    hotel_proxy <<- paste0("You will be residing at XX - located at XX - on the nights of DAY, MONTH DATE, YEAR and DAY, MONTH DATE, YEAR. ",pronoun_poss_cap," hotel confirmation is: XXXXXXXX.")
    } else {hotel <<- "";hotel_proxy<<-""}
    
    fu_transport_7yr <- pdb_data[i,"visit_transport_needed"]
    if (is.na(fu_transport_7yr)) {fu_transport_7yr<-"No"}
    if (fu_transport_7yr=="Yes") {t_need <<- "Your appointment will be held at the Vanderbilt University Medical Center. We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case you need to contact him."
    t_need_proxy <<- paste0(pronoun_poss_cap," appointment will be held at the Vanderbilt University Medical Center.  We will be providing transportation to and from ",pronoun_poss," visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case ",pronoun," needs to contact him.")
    } else {t_need <<- ""; t_need_proxy <<- ""}
    consent <<- paste0("Consent Statement. This document describes the VMAP Study. You completed this form when you originally came in for your eligibility visit, but we ask that you complete and sign this document again at each follow-up visit. Please read the form thoroughly before the appointment. We will ask you and your study partner, ",proxy_first_name,", to sign the consent form after we review it with you at the appointment. If your study partner does not plan to attend, we will mail them a copy to sign and return prior to your visit.")
    consent_prox <<- paste0("Consent Statement. This document describes the VMAP Study. You and ",first_name," already completed this form at the eligibility visit, but we ask that both of you sign this document again at each follow-up visit. Please read the form thoroughly. If you have questions, please let us know. If you will not be attending the visit, please return the signed copy prior to your visit.")
    
    if (pdb_data[i,"proxy_diff_address"] == "Yes") {
      partner <<- paste0("Study Partner Packet. We have mailed a packet of material to your study partner, ",proxy_first_name,", which we ask that ",proxy_pronoun," complete prior to your visit.")
      partner_prox <<- paste0("Study Partner Packet. This packet is for you. Please complete these questionnaires prior to ",pronoun_poss," visit. If you will not be attending, return the signed consent document and completed questionnaires in the included prepaid envelope.")
    } else {
      partner <<- paste0("Study Partner Packet. We have included a packet of material for your study partner, ",proxy_first_name,", which we ask that ",proxy_pronoun," complete prior to your visit.")
      partner_prox <<- paste0("Study Partner Packet. This packet is for you. Please complete these questionnaires prior to ",pronoun_poss," visit. If you will not be attending, return the signed consent document and completed questionnaires in the included prepaid envelope.")
    }
    
    field <- "visit_letter"
    field_proxy <- "visit_letter_proxy"
    
    path_in <- paste0("/srv/shiny-server/resources/Templates/Previsit/MAP_template.docx")
    temp <- "/app/Output/previsit_temp.docx"
    path_in_proxy <- paste0("/srv/shiny-server/resources/Templates/Previsit/MAP_proxy_template.docx")
    temp_proxy <- "/app/Output/previsit_temp_prox.docx"
    
    #################### EPOCH 5 Ends HERE
    
  } else {
    #################### EPOCH 0 Starts HERE ############################
    
    location_ptp <<- paste0("Your eligibility visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to greet you.")
    location_prox <<- paste0("The eligibility visit will be held at the Vanderbilt Memory and Alzheimer\'s Center, located on 1207 17th Avenue S., Suite 302. Upon arriving at the Vanderbilt Memory & Alzheimer\'s Center, please park in lot 128, to the right of the building, in a spot labeled \"VMAC Participant Parking\". Call 615-347-6937 and a team member will come down to greet you.")
    
    elig_date <<- format(as.Date(pdb_data[i, "elig_date"]), "%A, %B %d, %Y")
    elig_time <<- as.character(pdb_data[i,"elig_time"])
    
    elig_transport <- pdb_data[i,"visit_transport_needed"]
    if (is.na(elig_transport)) {elig_transport<-"No"}
    if (elig_transport=="Yes") {
      location_ptp <<- "Your eligibility visit will be held at the Vanderbilt Memory and Alzheimer's Center office.  We will be providing you with transportation to and from your visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case you need to contact him."
      location_prox <<- paste0("The eligibility will be held at the Vanderbilt Memory and Alzheimer's Center office.  We will be providing transportation to and from ",pronoun_poss," visit with Jeff Cornelius. Jeff\'s number is 615-604-1502 in case you need to contact him.")
    }
    consent <<- paste0("Consent Statement. This document describes the VMAP Study. Please read the form thoroughly before the appointment. If you have questions, please call 615-347-6937 or we can discuss your questions at your appointment. We will ask you and your study partner, ",proxy_first_name,", to sign the consent form after we review it with you at your eligibility visit appointment. ")
    consent_prox <<- paste0("Consent Statement. This document describes the VMAP Study. Please read the form thoroughly before the appointment. If you have questions, please call 615-347-6937 or we can discuss your questions at the appointment. We will ask you and ",first_name," to sign the consent form after we review it with you at your eligibility visit appointment. ")
    histor <<- paste0("Medical History Forms & Questionnaires. Prior to your eligibility appointment, please complete ALL questionnaires. Please be sure to bring all of your medications to the eligibility visit so our team can review them with you. ")
    
    if (pdb_data[i,"proxy_diff_address"] == "Yes") {
      partner <<- paste0("Study Partner Packet. We have mailed a packet of material to your study partner, ",proxy_first_name,".")
      partner_prox <<- paste0("Study Partner Packet. Prior to the appointment, please complete ALL questionnaires and bring the completed packet to ",pronoun_poss," eligibility visit.")
    } else {
      partner <<- paste0("Study Partner Packet. We have included a packet of material for your study partner, ",proxy_first_name,".")
      partner_prox <<- paste0("Study Partner Packet. Prior to the appointment, please complete ALL questionnaires and bring the completed packet to ",pronoun_poss," eligibility visit.")
    }
    
    field <- paste0("elig_letter")
    field_proxy <- paste0("elig_letter_proxy")
    
    path_in <- paste0("/srv/shiny-server/resources/Templates/Previsit/MAP_elig_template.docx")
    path_in_proxy <- paste0("/srv/shiny-server/resources/Templates/Previsit/MAP_elig_proxy_template.docx")
  }
  
  is.even <- function(x) x %% 2 == 0
  
  if (e > 0) {
    #################### Itinerary Tables ###############
    
    # EPOCH 5 Itinerary Tables
    
    even <- c(2,4,6,8,10)
    odd <- c(1,3,5,7,9)
    
    locat <- c("VMAC","Vanderbilt Hospital Valet","your home","VMAC")
    location1 <- locat[loc_day1]
    
    if (X==1) {
      df <- data.frame(
        Day1 = c(paste0("Arrival at ",location1," at ",visit1_time),
                 "Consent Process",
                 "Paperwork Review",
                 "Blood Work & Physical Exam",
                 "Breakfast",
                 "Memory Testing",
                 "Heart MRI",
                 "Echo & Lunch Break",
                 "Clinical Interview",
                 "Brain MRI",
                 paste0("Study Wrap-up at ",visit1_time," plus ",visit1_hours)))
      ft <- flextable(df) # 1-Day
      ft <- set_header_labels(ft, Day1 = "Itinerary: 1 Day Visit")
      ft <- bg(ft, bg="grey",part = "header")
      ft <- bg(ft, i=c(2,4,6,8,10),bg="grey",part = "body")
      ft <- font(ft,fontname = 'Arial')
      ft <- font(ft,fontname = 'Arial',part = "header")
      ft <- fontsize(ft,size = 13)
      ft <- fontsize(ft,size = 13,part = "header")
      ft <- width(ft,width = 4.5)
      ft <- theme_box(ft)
      ft <- bold(ft,bold = TRUE,part = "body")
      ft <<- align(ft, align = "center", part="header")
      df1 <- data.frame(c("Remove this table"))
      ft1 <<- flextable(df)
      df2 <- data.frame(c("Remove this table"))
      ft2 <<- flextable(df)
      FT <<- list(ft = ft, ft1 = ft1, ft2 = ft2)
    }
    
    if (X==2 | X==3) {
      location2 <- locat[loc_day2]
      df <- data.frame(c("Remove this table"))
      ft <<- flextable(df)
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
                 "Blood Work & Physical Exam",
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
      
      FT <<- list(ft = ft, ft1 = ft1, ft2 = ft2)
    }
    
    body_add_flextables(path_in,temp, FT)
    body_add_flextables(path_in_proxy,temp_proxy, FT)
    
    # End of Itinerary Tables
  } else {temp <- path_in;temp_proxy <- path_in_proxy}
  
  output <- paste0("/app/Output/VMAC_",input,"_",ep,"_ptp_letter.docx")
  renderInlineCode(temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = field, event = pdb_data[,"redcap_event_name"],
              overwrite = TRUE, repeat_instance = 1)
  
  if (is.na(proxy_first_name)==FALSE) {
    output_proxy <- paste0("/app/Output/VMAC_",input,"_",ep,"_proxy_letter.docx")
    renderInlineCode(temp_proxy, output_proxy)
    
    importFiles(rcon = pdb, file = output_proxy, record = record, field = field_proxy, event = pdb_data[,"redcap_event_name"],
                overwrite = TRUE, repeat_instance = 1)
  }
    
    #}
  #}
  
}