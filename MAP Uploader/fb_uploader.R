fb_uploader<<- function(epochh,map) {
  
  library(ggplot2)
  library(plotly)
  library(redcapAPI)
  library(WordR)
  library(officer)
  library(Hmisc)
  library(tidyverse)
  library(flextable)
  library(ggpattern)
  
  FII <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "489C53D4DAAE99F87EF37A9D77563BB0",conn,project = 42471)
  
  tm7yr <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                            token = "1769F48432D599795BAA88493EFFCF3F",conn,project = 117460)
  
  tm60 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "54A5137E46FFC775D9BC1117DA32B30A",conn,project = 80145)
  
  tm36 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "3CCD5E3C8A09BE5F6E940B291A9BC6B4",conn,project = 73222)
  
  tm18 <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                           token = "04661B3930D4F892AB1F194C287EE98B",conn,project = 56822)
  
  tme <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "1A5DACDD0A80C92D60FCCC27A501286E",conn,project = 39332)
  
  pdb <- redcapConnection(url = "https://redcap.vanderbilt.edu/api/",
                          token = "CBF02E285BFC1874F0EAF11D3F4E2842", conn, project = 23166)
  
  pdb_datas <- exportReports(pdb, 241869)
  #try(pdb_datas <- exportReports(pdb, 241869), silent = TRUE)
#if (exists("pdb_datas")==FALSE) {print("No Updates")} else {
  tm7yr_datas <- exportReports(tm7yr, 248514)
  tm60_datas <- exportReports(tm60, 248512)
  tm36_datas <- exportReports(tm36, 248500)
  tme_datas <- exportReports(tme, 248431)
  fii36s <- exportReports(FII, 252306)
  fii60s <- exportReports(FII, 252308)
  fii7yrs <- exportReports(FII, 252309)
  
  fii60s[which(is.na(fii60s["extracardiac_incidental"])),"extracardiac_incidental"]<- "No"
  fii60s[which(is.na(fii60s["brain_incidental"])),"brain_incidental"]<- "No"
  fii7yrs[which(is.na(fii7yrs["brain_incidental"])),"brain_incidental"]<- "No"
  fii7yrs[which(is.na(fii7yrs["extracardiac_incidental"])),"extracardiac_incidental"]<- "No"
  fii36s[which(is.na(fii36s["extracardiac_incidental"])),"extracardiac_incidental"]<- "No"
  fii36s[which(is.na(fii36s["brain_incidental"])),"brain_incidental"]<- "No"
  
  #for (i in 1:nrow(pdb_datas)) {
  
  i <- which(pdb_datas["map_id"]==as.integer(map))
  
  pdb_data <- pdb_datas[i,]
  pdb_data[,which(is.na(pdb_data[,"feedback_location_7yr"]))]<-"other"
  #pdb_data[,which(is.na(pdb_data))]<- "Missing"
  
  # Epoch Selector
  #ep_sel <- c(is.na(pdb_data$fu_date_18mos),is.na(pdb_data$fu_date_36mos),is.na(pdb_data$fu_date_60mos),is.na(pdb_data$fu_date_7yr),is.na(pdb_data$fu_date_9yr)) 
  e <- epochh
  Epoch_conv <- c("Enrollment","18-Month","3-Year","5-Year","7-Year","9-Year","11-Year","13-Year")
  Epoc_conv <- c("enrollment","18-month","3-year","5-year","7-year","9-year","11-year","13-year")
  Epoch <<- Epoch_conv[e]; Epoc <<- Epoc_conv[e]
  Epoch2 <<- Epoch_conv[e-2]; Epoc2 <<- Epoc_conv[e-2]
  Epoch1 <<- Epoch_conv[e-1]; Epoc1 <<- Epoc_conv[e-1]
  epoch_conv <- c("18mos","36mos","60mos","7yr","9yr","11yr","13yr")
  epoch2 <<- epoch_conv[e-3]
  epoch1 <<- epoch_conv[e-2]
  epoch <<- epoch_conv[e-1]
  ep_next <<- epoch_conv[e]
  ep <- epoch
  date_next <<- paste0("fu_date_estimate_",ep_next)
  date_ty <<- format(as.Date(pdb_data[, date_next]), "%B %Y")
  
  
  map_id <- as.character(pdb_data[,"map_id"])
  ind <- as.integer(map_id)
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(tm7yr_datas[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  tm7yr_datas[inddd,which(is.na(tm7yr_datas[inddd,]))]<- "Missing"
  tm7yr_data <- tm7yr_datas[inddd,]
  if (nrow(tm7yr_data)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(tm60_datas[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  tm60_datas[inddd,which(is.na(tm60_datas[inddd,]))]<- "Missing"
  tm60_data <- tm60_datas[inddd,]
  if (nrow(tm60_data)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(tm36_datas[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  tm36_datas[inddd,which(is.na(tm36_datas[inddd,]))]<- "Missing"
  tm36_data <- tm36_datas[inddd,]
  if (nrow(tm36_data)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(tme_datas[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  tme_datas[inddd,which(is.na(tme_datas[inddd,]))]<- "Missing"
  tme_data <- tme_datas[inddd,]
  if (nrow(tme_data)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(fii36s[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  fii36s[inddd,which(is.na(fii36s[inddd,]))]<- "Missing"
  fii36 <- fii36s[inddd,]
  if (nrow(fii36)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(fii60s[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  fii60s[inddd,which(is.na(fii60s[inddd,]))]<- "Missing"
  fii60 <- fii60s[inddd,]
  if (nrow(fii60)==FALSE) {stop("Not Enough Data")}
  
  inddd <- c()
  if (exists("indd")==TRUE) remove("indd")
  try(indd <- find.matches(fii7yrs[, "map_id"],ind),silent = TRUE)
  if (exists("indd")==FALSE) stop("Not Enough Data")
  for (i in (1:length(indd$matches))){if (indd$matches[i]>0){inddd<-c(inddd,i)}}
  fii7yrs[inddd,which(is.na(fii7yrs[inddd,]))] <- "Missing"
  fii7yr <- fii7yrs[inddd,]
  if (nrow(fii7yr)==FALSE) {stop("Not Enough Data")}
  
  i<-1
  
  enroll_date <<- format(as.Date(pdb_data[i, "enroll_date"]), "%m/%d/%Y")
  fu_date_36mos <<- format(as.Date(pdb_data[i, "fu_date_36mos"]), "%m/%d/%Y")
  fu_date_60mos <<- format(as.Date(pdb_data[i, "fu_date_60mos"]), "%m/%d/%Y")
  fu_date_7yr <<- format(as.Date(pdb_data[i, "fu_date_7yr"]), "%m/%d/%Y")
  
  first_name <<- pdb_data[i,"preferred_name"]
  last_name <<- pdb_data[i, "last_name"]
  street_address <<- pdb_data[i, "street_address"]
  city <<- pdb_data[i, "city"]
  state <<- pdb_data[i, "state"]
  zipp <<- pdb_data[i, "zip"]
  dob <<- format(as.Date(pdb_data[i, "dob"]), "%m/%d/%Y")
  sex <<- as.character(pdb_data[i, "sex"])
  gender <- ""
  if (sex=="Female") {gender<<- "women"} else {gender <<- "men"}
  if (sex=="Female") {gender_cap<<- "Women"} else {gender_cap <<- "Men"}
  pronoun_conv<<- c("Female" = "she", "Male" = "he")
  pronoun_conv_obj<<- c("Female" = "her", "Male" = "him")
  pronoun_conv_poss<<- c("Female" = "her", "Male" = "his")
  pronoun_conv_poss_cap<<- c("Female" = "Her", "Male" = "His")
  pronoun<<- pronoun_conv[sex]
  pronoun_obj<<- pronoun_conv_obj[sex]
  pronoun_poss<<- pronoun_conv_poss[sex]
  pronoun_poss_cap<<- pronoun_conv_poss_cap[sex]
  salutation<<- as.character(pdb_data[i, "salutation"])
  fb_date1_7yr<<- pdb_data[i, "feedback_date_7yr"]
  if (is.na(fb_date1_7yr)) {feedback_date1_7yr<<- "UNKNOWN"} else {feedback_date1_7yr<<- format(as.Date(fb_date1_7yr), "%m/%d/%Y")}
  feedback_location_7yr<<- as.character(pdb_data[i, "feedback_location_7yr"])
  
  df <- data.frame(
    Test = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
    Test.1 = c("Heart rate", "Blood pressure", "Height", "Weight", "Body Mass Index"),
    ER = c(tme_data[i, 2], tme_data[i, 3], paste0(pdb_data[i, "height_total"]," inches"), paste0(pdb_data[i, "weight"]," lbs"), round(((pdb_data[i, "weight"]/2.205)/((pdb_data[i, "height_total"]/0.393701)/100)^2), digits=1)),
    ER = c(tme_data[i, 2], tme_data[i, 4], paste0(pdb_data[i, "height_total"]," inches"), paste0(pdb_data[i, "weight"]," lbs"), round(((pdb_data[i, "weight"]/2.205)/((pdb_data[i, "height_total"]/0.393701)/100)^2), digits=1)),
    MR_36 = c(tm36_data[i, 2], fii36[i, 4], paste0(round(pdb_data[i, "height_36mos"]*0.393701), " inches"), paste0(round(pdb_data[i, "weight_36mos"]*2.205), " lbs"), round((pdb_data[i, "weight_36mos"]/(pdb_data[i, "height_36mos"]/100)^2), digits=1)),
    MR_36 = c(tm36_data[i, 2], fii36[i, 5], paste0(round(pdb_data[i, "height_36mos"]*0.393701), " inches"), paste0(round(pdb_data[i, "weight_36mos"]*2.205), " lbs"), round((pdb_data[i, "weight_36mos"]/(pdb_data[i, "height_36mos"]/100)^2), digits=1)),
    MR_60 = c(tm60_data[i, 2], fii60[i, 4], paste0(round(as.integer(pdb_data[i, "height_60mos"])*0.393701), " inches"), paste0(round(as.integer(pdb_data[i, "weight_60mos"])*2.205), " lbs"), round((as.integer(pdb_data[i, "weight_60mos"])/(as.integer(pdb_data[i, "height_60mos"])/100)^2), digits=1)),
    MR_60 = c(tm60_data[i, 2], fii60[i, 5], paste0(round(as.integer(pdb_data[i, "height_60mos"])*0.393701), " inches"), paste0(round(as.integer(pdb_data[i, "weight_60mos"])*2.205), " lbs"), round((as.integer(pdb_data[i, "weight_60mos"])/(as.integer(pdb_data[i, "height_60mos"])/100)^2), digits=1)),
    CR = c(tm7yr_data[i, 2], fii7yr[i, 4], paste0(round(as.integer(pdb_data[i, "height_7yr"])*0.393701)," inches"), paste0(round(as.integer(pdb_data[i, "weight_7yr"])*2.205)," lbs"), round((as.integer(pdb_data[i, "weight_7yr"])/(as.integer(pdb_data[i, "height_7yr"])/100)^2), digits=1)),
    CR = c(tm7yr_data[i, 2], fii7yr[i, 5], paste0(round(as.integer(pdb_data[i, "height_7yr"])*0.393701)," inches"), paste0(round(as.integer(pdb_data[i, "weight_7yr"])*2.205)," lbs"), round((as.integer(pdb_data[i, "weight_7yr"])/(as.integer(pdb_data[i, "height_7yr"])/100)^2), digits=1)),
    NR = c("60-100", "<120 / <80", "n/a", "n/a", "18.5-24.9")
  )
  
  ft <- flextable(df)
  ft <- set_header_labels(ft, Test = "Test", Test.1="Test",
                          ER = paste0("Enrollment Results ",enroll_date),ER.1 = paste0("Enrollment Results ",enroll_date), 
                          MR_36 = paste0(Epoch2," Results ", fu_date_36mos),MR_36.1 = paste0(Epoch2," Results ", fu_date_36mos),
                          MR_60 = paste0(Epoch1," Results ", fu_date_60mos),MR_60.1 = paste0(Epoch1," Results ", fu_date_60mos), 
                          CR = paste0("Current ",Epoch," Results ", fu_date_7yr),CR.1 = paste0("Current ",Epoch," Results ", fu_date_7yr),
                          NR = "Normal Range*" )
  ft <- bg(ft, bg="grey",part = "header")
  ft <- fontsize(ft, j=1:11, size = 10, part="header")
  ft <- fontsize(ft, j=1:11, size = 10, part="body")
  ft <- width(ft,j = 11, width = 1)
  
  ft <- merge_h(ft,part = "header")
  ft <- merge_h_range(ft,i = 1:5,j1=1, j2=2 ,part = "body")
  ft <- merge_h_range(ft,i = 1,j1=3, j2=4 ,part = "body")
  ft <- merge_h_range(ft,i = 1,j1=5, j2=6 ,part = "body")
  ft <- merge_h_range(ft,i = 1,j1=7, j2=8 ,part = "body")
  ft <- merge_h_range(ft,i = 1,j1=9, j2=10 ,part = "body")
  ft <- merge_h_range(ft,i = 3:5,j1=3, j2=4 ,part = "body")
  ft <- merge_h_range(ft,i = 3:5,j1=5, j2=6 ,part = "body")
  ft <- merge_h_range(ft,i = 3:5,j1=7, j2=8 ,part = "body")
  ft <- merge_h_range(ft,i = 3:5,j1=9, j2=10 ,part = "body")
  ft <- width(ft,j = 3:10, width = .5)
  ft <- width(ft, j=1:2, width = .75)
  
  ft <- theme_box(ft)
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, align = "center", part="body")
  ft <- align(ft, i = 1:5, j = 1:2, align="left",part="body")
  for (ii in 3:(ncol(df)-1)) {if ((as.integer(df[1,ii]) < 60) | (as.integer(df[1,ii]) >100)) {ft <- bold(ft, i = 1, j = ii, bold = TRUE, part = "body")}
  if (as.integer(df[2,ii]) > 120) {ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}
  if ((as.double(df[5,ii]) < 18.5) | (as.double(df[5,ii]) > 24.9)) {ft <- bold(ft, i = 5, j = ii, bold = TRUE, part = "body")}}
  evens <- c(4,6,8,10)
  for (ii in evens) {if (as.integer(df[2,ii]) > 80) {ft <- bold(ft, i = 2, j = ii, bold = TRUE, part = "body")}}
  
  df2 <- data.frame(
    Test1 = c("Cholesterol", "Cholesterol", "Cholesterol", "Cholesterol", "Blood Sugar", "Blood Sugar", "Blood Sugar", "Thyroid", "Inflammation"),
    Test2 = c("Total","HDL", "LDL", "Triglycerides", "Hemoglobin A1C", "Fasting Insulin", "Fasting Glucose", "Thyroid Stimulating Hormone (TSH)", "High Sensitivity C-Reactive Protein"),
    ER = c(tme_data[i, 5], tme_data[i, 6], tme_data[i,7], tme_data[i,8], tme_data[i,9], tme_data[i,10], tme_data[i,11], tme_data[i,12], tme_data[i,13]),
    MR_36 = c(tm36_data[i, 5], tm36_data[i, 6], tm36_data[i,7], tm36_data[i,8], tm36_data[i,9], tm36_data[i,10], tm36_data[i,11], tm36_data[i,12], tm36_data[i,13]),
    MR_60 = c(tm60_data[i, 5], tm60_data[i, 6], tm60_data[i,7], tm60_data[i,8], tm60_data[i,9], tm60_data[i,10], tm60_data[i,11], tm60_data[i,12], tm60_data[i,13]),
    CR = c(tm7yr_data[i, 5], tm7yr_data[i, 6], tm7yr_data[i,7], tm7yr_data[i,8], tm7yr_data[i,9], tm7yr_data[i,10], tm7yr_data[i,11], tm7yr_data[i,12], tm7yr_data[i,13]),
    NR = c("<200", "men >40, women >50", "<100", "<150", "4-6.5", "<17.2", "70-110", "0.3-5.0", "0.1-3.0")
  )
  
  ft2 <- flextable(df2)
  ft2 <- width(ft2, j = 1:7, width=.9)
  ft2 <- set_header_labels(ft2, Test1 = "Test", Test2 = "Test", ER = paste0("Enrollment Results ",enroll_date), 
                           MR_36 = paste0(Epoch2," Results ", fu_date_36mos),
                           MR_60 = paste0(Epoch1," Results ", fu_date_60mos), CR = paste0("Current ",Epoch," Results ", fu_date_7yr), NR = "Normal Range/\nCut-off*" )
  ft2 <- merge_at(ft2, i = 1, j = 1:2, part = "header")
  ft2 <- merge_at(ft2, i = 1:4, j = 1, part = "body")
  ft2 <- merge_at(ft2, i = 5:7, j = 1, part = "body")
  ft2 <- bg(ft2, bg="grey",part = "header")
  ft2 <- fontsize(ft2, j=1, size = 9, part="body")
  ft2 <- fontsize(ft2, j=1:7, size = 10, part="header")
  ft2 <- fontsize(ft2, j=2:7, size = 10, part="body")
  ft2 <- theme_box(ft2)
  ft2 <- align(ft2, align = "center", part="header")
  ft2 <- align(ft2, align = "center", part="body")
  ft2 <- align(ft2, i=1:9, j=2, align="left",part="body")
  ft2 <- valign(ft2, i=1:9, j=3:7, valign="top", part="body")
  ft2 <- height(ft2, height = .4, part = "header")
  ft2 <- width(ft2, j = 1, width = .85)
  ft2 <- width(ft2, j = 2, width = 1.25)
  for (ii in 3:(ncol(df2)-1)) {if (as.integer(df2[1,ii]) > 200) {ft2 <- bold(ft2, i = 1, j = ii, bold = TRUE, part = "body")}
    if (if (sex == "Female"){as.integer(df2[2,ii]) < 50} else {as.integer(df2[2,ii]) < 40}) {ft2 <- bold(ft2, i = 2, j = ii, bold = TRUE, part = "body")}
    if (as.integer(df2[3,ii]) > 100) {ft2 <- bold(ft2, i = 3, j = ii, bold = TRUE, part = "body")}
    if (as.integer(df2[4,ii]) > 150) {ft2 <- bold(ft2, i = 4, j = ii, bold = TRUE, part = "body")}
    if ((as.integer(df2[5,ii]) > 6.5) | (as.integer(df2[5,ii]) < 4)) {ft2 <- bold(ft2, i = 5, j = ii, bold = TRUE, part = "body")}
    if (as.integer(df2[6,ii]) > 17.2) {ft2 <- bold(ft2, i = 6, j = ii, bold = TRUE, part = "body")}
    if ((as.integer(df2[7,ii]) > 110) | (as.integer(df2[7,ii]) < 70)) {ft2 <- bold(ft2, i = 7, j = ii, bold = TRUE, part = "body")}
    if ((as.double(df2[8,ii]) > 5) | (as.double(df2[8,ii]) < 0.3)) {ft2 <- bold(ft2, i = 8, j = ii, bold = TRUE, part = "body")}
  }
  
  
  first_name_physician1<<- pdb_data[i, "first_name_physician1_7yr"]
  last_name_physician1<<- pdb_data[i, "last_name_physician1_7yr"]
  credentials1<<- pdb_data[i,"credentials_physician1_7yr"]
  street_address_physician1<<- pdb_data[i, "street_address_physician1_7yr"]
  city_physician1<<- pdb_data[i, "city_physician1_7yr"]
  state_physician1<<- pdb_data[i, "state_physician1_7yr"]
  zip_physician1<<- pdb_data[i, "zip_physician1_7yr"]
  first_name_physician2<<- pdb_data[i, "first_name_physician2_7yr"]
  last_name_physician2<<- pdb_data[i, "last_name_physician2_7yr"]
  credentials2<<- pdb_data[i,"credentials_physician2_7yr"]
  street_address_physician2<<- pdb_data[i, "street_address_physician2_7yr"]
  city_physician2<<- pdb_data[i, "city_physician2_7yr"]
  state_physician2<<- pdb_data[i, "state_physician2_7yr"]
  zip_physician2<<- pdb_data[i, "zip_physician2_7yr"]
  first_name_physician3<<- pdb_data[i, "first_name_physician3_7yr"]
  last_name_physician3<<- pdb_data[i, "last_name_physician3_7yr"]
  credentials3<<- pdb_data[i,"credentials_physician3_7yr"]
  street_address_physician3<<- pdb_data[i, "street_address_physician3_7yr"]
  city_physician3<<- pdb_data[i, "city_physician3_7yr"]
  state_physician3<<- pdb_data[i, "state_physician3_7yr"]
  zip_physician3<<- pdb_data[i, "zip_physician3_7yr"]
  first_name_physician4<<- pdb_data[i, "first_name_physician4_7yr"]
  last_name_physician4<<- pdb_data[i, "last_name_physician4_7yr"]
  credentials4<<- pdb_data[i,"credentials_physician4_7yr"]
  street_address_physician4<<- pdb_data[i, "street_address_physician4_7yr"]
  city_physician4<<- pdb_data[i, "city_physician4_7yr"]
  state_physician4<<- pdb_data[i, "state_physician4_7yr"]
  zip_physician4<<- pdb_data[i, "zip_physician4_7yr"]
  first_name_physician5<<- pdb_data[i, "first_name_physician5_7yr"]
  last_name_physician5<<- pdb_data[i, "last_name_physician5_7yr"]
  street_address_physician5<<- pdb_data[i, "street_address_physician5_7yr"]
  credentials5<<- pdb_data[i,"credentials_physician5_7yr"]
  city_physician5<<- pdb_data[i, "city_physician5_7yr"]
  state_physician5<<- pdb_data[i, "state_physician5_7yr"]
  zip_physician5<<- pdb_data[i, "zip_physician5_7yr"]
  
  if (is.na(first_name_physician1)) {first_name_physician1_7yr<<- ""} else {first_name_physician1_7yr<<- paste0("     1.  ",pdb_data[i, "first_name_physician1_7yr"])}
  if (is.na(last_name_physician1)) {last_name_physician1_7yr<<- ""} else {last_name_physician1_7yr<<- paste0(pdb_data[i, "last_name_physician1_7yr"],", MD")}
  if (is.na(street_address_physician1)) {street_address_physician1_7yr<<- ""} else {street_address_physician1_7yr<<- paste0("          ",pdb_data[i, "street_address_physician1_7yr"])}
  if (is.na(city_physician1)) {city_physician1_7yr<<- ""} else {city_physician1_7yr<<- paste0("          ",pdb_data[i, "city_physician1_7yr"],",")}
  state_physician1_7yr<<- pdb_data[i, "state_physician1_7yr"]
  if (is.na(state_physician1_7yr)) {state_physician1_7yr<<- ""}
  zip_physician1_7yr<<- pdb_data[i, "zip_physician1_7yr"]
  if (is.na(zip_physician1_7yr)) {zip_physician1_7yr<<- ""}
  if (is.na(first_name_physician2)) {first_name_physician2_7yr<<- ""} else {first_name_physician2_7yr<<- paste0("     2.  ",pdb_data[i, "first_name_physician2_7yr"])}
  if (is.na(last_name_physician2)) {last_name_physician2_7yr<<- ""} else {last_name_physician2_7yr<<- paste0(pdb_data[i, "last_name_physician2_7yr"],", MD")}
  if (is.na(street_address_physician2)) {street_address_physician2_7yr<<- ""} else {street_address_physician2_7yr<<- paste0("          ",pdb_data[i, "street_address_physician2_7yr"])}
  if (is.na(city_physician2)) {city_physician2_7yr<<- ""} else {city_physician2_7yr<<- paste0("          ",pdb_data[i, "city_physician2_7yr"],",")}
  state_physician2_7yr<<- pdb_data[i, "state_physician2_7yr"]
  if (is.na(state_physician2_7yr)) {state_physician2_7yr<<- ""}
  zip_physician2_7yr<<- pdb_data[i, "zip_physician2_7yr"]
  if (is.na(zip_physician2_7yr)) {zip_physician2_7yr<<- ""}
  if (is.na(first_name_physician3)) {first_name_physician3_7yr<<- ""} else {first_name_physician3_7yr<<- paste0("     3.  ",pdb_data[i, "first_name_physician3_7yr"])}
  if (is.na(last_name_physician3)) {last_name_physician3_7yr<<- ""} else {last_name_physician3_7yr<<- paste0(pdb_data[i, "last_name_physician3_7yr"],", MD")}
  if (is.na(street_address_physician3)) {street_address_physician3_7yr<<- ""} else {street_address_physician3_7yr<<- paste0("          ",pdb_data[i, "street_address_physician3_7yr"])}
  if (is.na(city_physician3)) {city_physician3_7yr<<- ""} else {city_physician3_7yr<<- paste0("          ",pdb_data[i, "city_physician3_7yr"],",")}
  state_physician3_7yr<<- pdb_data[i, "state_physician3_7yr"]
  if (is.na(state_physician3_7yr)) {state_physician3_7yr<<- ""}
  zip_physician3_7yr<<- pdb_data[i, "zip_physician3_7yr"]
  if (is.na(zip_physician3_7yr)) {zip_physician3_7yr<<- ""}
  if (is.na(first_name_physician4)) {first_name_physician4_7yr<<- ""} else {first_name_physician4_7yr<<- paste0("     3.  ",pdb_data[i, "first_name_physician4_7yr"])}
  if (is.na(last_name_physician4)) {last_name_physician4_7yr<<- ""} else {last_name_physician4_7yr<<- paste0(pdb_data[i, "last_name_physician4_7yr"],", MD")}
  if (is.na(street_address_physician4)) {street_address_physician4_7yr<<- ""} else {street_address_physician4_7yr<<- paste0("          ",pdb_data[i, "street_address_physician4_7yr"])}
  if (is.na(city_physician4)) {city_physician4_7yr<<- ""} else {city_physician4_7yr<<- paste0("          ",pdb_data[i, "city_physician4_7yr"],",")}
  state_physician4_7yr<<- pdb_data[i, "state_physician4_7yr"]
  if (is.na(state_physician4_7yr)) {state_physician4_7yr<<- ""}
  zip_physician4_7yr<<- pdb_data[i, "zip_physician4_7yr"]
  if (is.na(zip_physician4_7yr)) {zip_physician4_7yr<<- ""}
  if (is.na(first_name_physician5)) {first_name_physician5_7yr<<- ""} else {first_name_physician5_7yr<<- paste0("     3.  ",pdb_data[i, "first_name_physician5_7yr"])}
  if (is.na(last_name_physician5)) {last_name_physician5_7yr<<- ""} else {last_name_physician5_7yr<<- paste0(pdb_data[i, "last_name_physician5_7yr"],", MD")}
  if (is.na(street_address_physician5)) {street_address_physician5_7yr<<- ""} else {street_address_physician5_7yr<<- paste0("          ",pdb_data[i, "street_address_physician5_7yr"])}
  if (is.na(city_physician5)) {city_physician5_7yr<<- ""} else {city_physician5_7yr<<- paste0("          ",pdb_data[i, "city_physician5_7yr"],",")}
  state_physician5_7yr<<- pdb_data[i, "state_physician5_7yr"]
  if (is.na(state_physician5_7yr)) {state_physician5_7yr<<- ""}
  zip_physician5_7yr<<- pdb_data[i, "zip_physician5_7yr"]
  if (is.na(zip_physician5_7yr)) {zip_physician5_7yr<<- ""}
  
  matt<- rbind(tme_data[-(1:17)],tm36_data[-(1:17)],tm60_data[-(1:17)],tm7yr_data[-(1:17)][-19])
  missing<- matt==-9999
  for (ii in 1:ncol(matt)) {for (j in 1:nrow(matt)) {if (missing[j,ii]) {matt[j,ii]<-matt[j-1,ii]}}}
  
  # Initialize memory variables
  np_cvlt1to5_tscore<<- c(matt[1])
  np_cvlt_sdfr_z<<- c(matt[2])
  np_cvlt_ldfr_z<<- c(matt[3])
  np_cvltrecog_discrim_z<<- c(matt[4])
  np_biber_t1to5_z<<- c(matt[5])
  np_biber_sd_z<<- c(matt[6])
  np_biber_ld_z<<- c(matt[7])
  np_anim_tscore<<- c(matt[8])
  np_bnt_z<<- c(matt[9])
  np_tower_ss<<- c(matt[10])
  np_inhibit_ss<<- c(matt[11])
  np_fas_tscore<<- c(matt[12])
  np_tmtb_ss<<- c(matt[13])
  np_hvot_tscore<<- c(matt[14])
  np_digsymb_ss<<- c(matt[15])
  np_color_ss<<- c(matt[16])
  np_word_ss<<- c(matt[17])
  np_tmta_ss<<- c(matt[18])
  
  # transform variables
  np_cvlt1to5_z<<- (np_cvlt1to5_tscore[[1]] - 50)/10
  np_anim_z<<- (np_anim_tscore[[1]]-50)/10
  np_tower_z<<- (np_tower_ss[[1]]-10)/3
  np_inhibit_z<<- (np_inhibit_ss[[1]]-10)/3
  np_fas_z<<- (np_fas_tscore[[1]]-50)/10
  np_tmtb_z<<- (np_tmtb_ss[[1]]-10)/3
  np_hvot_z<<- -(np_hvot_tscore[[1]]-50)/10
  np_digsymb_z<<- (np_digsymb_ss[[1]]-10)/3
  np_color_z<<- (np_color_ss[[1]]-10)/3
  np_word_z<<- (np_word_ss[[1]]-10)/3
  np_tmta_z<<- (np_tmta_ss[[1]]-10)/3
  
  # composite scores
  mem_w<<- cbind(np_cvlt1to5_z,np_cvlt_sdfr_z[[1]],np_cvlt_ldfr_z[[1]],np_cvltrecog_discrim_z[[1]])
  memory_words<<- rowMeans(mem_w)
  mem_s<<- cbind(np_biber_t1to5_z[[1]], np_biber_sd_z[[1]], np_biber_ld_z[[1]])
  memory_shapes<<- rowMeans(mem_s)
  lang<<- cbind(np_anim_z,np_bnt_z[[1]])
  language<- rowMeans(lang)
  multitasking<<- rowMeans(cbind(np_tower_z, np_inhibit_z, np_fas_z, np_tmtb_z))
  visuospatial<<- np_hvot_z
  attention<- rowMeans(cbind(np_digsymb_z, np_color_z, np_word_z, np_tmta_z))
  
  counts<- c(memory_words[1:4],memory_shapes[1:4],language[1:4],multitasking[1:4],visuospatial[1:4],attention[1:4])
  
  specie<- c(rep("Memory for\n Words",4),rep("Memory for\n Shapes",4),rep("Language",4),rep("Multi-tasking\n and planning",4),rep("Visuospatial\nskills",4),rep("Attention",4))
  condition<- rep(c(enroll_date, fu_date_36mos, fu_date_60mos, fu_date_7yr),6)
  level_order<- c("Memory for\n Words","Memory for\n Shapes","Language","Multi-tasking\n and planning","Visuospatial\nskills","Attention")
  cond_order<- c(enroll_date, fu_date_36mos, fu_date_60mos, fu_date_7yr)
  spec<- factor(specie,levels = level_order)
  cond<- factor(condition,levels = cond_order)
  data<- data.frame(specie,condition,counts)
  countss<- counts+3
  patterns<- rep(c('crosshatch','circle','none','crosshatch'),6)
  pattern_factor<- c('crosshatch','stripe','circle','none')
  patternss<- factor(patterns,levels = pattern_factor)
  for (ii in 1:length(countss)) {if (countss[ii]<0.25) {countss[ii]<-0.25}}
  
  
  barr <- ggplot(data,aes(fill=cond, y=countss, x=spec,pattern_key_scale_factor=patterns)) + theme_bw(14)+scale_fill_manual(values = c("black","white","white","black"))+
    geom_bar_pattern(width = .75,color=rep(c("black"),24),aes(pattern=cond),pattern_density=rep(c(0.3,0.2,0.2,0.2),6),pattern_colour=rep(c("black","black","black","white"),6),pattern_fill = rep(c("white","black","black","white"),6),pattern_spacing=.01,position="dodge", stat="identity",pattern_key_scale_factor = 1) +
    scale_y_continuous(name=NULL,breaks=c(1.5,3,4.5),labels = c("Below\nNormal","Normal","Above\nNormal"),limits = c(0,6),expand = c(0, 0))+
    scale_x_discrete(name=NULL) + theme(panel.spacing.x = unit(1,"lines"),plot.background = element_rect(fill = "white",colour = "black",size = 1),plot.margin = unit(c(.3, .8, .3, .3), "cm"),legend.key.size = unit(.21, 'cm'),panel.grid.major.y = element_line(c(1.5,3,4.5),color=c("black","black","black")),panel.grid.minor.y=element_blank(),panel.grid.major.x=element_blank(),axis.text = element_text(colour = "black"),plot.title = element_text(size = 14,face = "bold",hjust = 0.5),axis.ticks = element_blank(),legend.position = "bottom", legend.title = element_blank(),legend.box.margin=margin(-15,0,0,0))+
    coord_fixed(ratio = .4) + labs(title = "Memory Testing Results")
  
  ptp_temp<<- paste0("C:/Users/sweelyb/Documents/resources/output/ptp_temp.docx")
  phys_temp<<- paste0("C:/Users/sweelyb/Documents/resources/output/phys_temp.docx")
  Plots<<- list(membar = function() print(barr))
  ptp_path<<- paste0("C:/Users/sweelyb/Documents/resources/Templates/Feedback/fb_letter_template_",epoch,".docx")
  phys_path<<- paste0("C:/Users/sweelyb/Documents/resources/Templates/Feedback/physician_temp_",epoch,".docx")
  addPlots(ptp_path,ptp_temp,Plots,width = 7.3,height = 3.6)
  addPlots(phys_path,phys_temp,Plots,width = 7.3,height = 3.6)
  FT<<- list(ft = ft, ft2 = ft2)
  body_add_flextables(ptp_temp,ptp_temp, FT)
  body_add_flextables(phys_temp,phys_temp, FT)
  
  lvd36<<- as.character(fii36[i,6])
  val36<<- as.character(fii36[i,7])
  if (val36=="Normal"){val36<<-"     3.  No significant"} else {val36<<-"     3.  Significant"}
  lvs36<<- as.character(fii36[i,8])
  if (is.na(lvs36)) {
    lvs36<<-""
    lvd36<<- paste0("     1.  ", lvd36)
  } else {lvs36<<- paste0("     1.  ",as.character(fii36[i,8])," left ventricular systolic function; ")}
  rv36<<- as.character(fii36[i,9])
  if (is.na(rv36)) {
    rv36<<- ""
  } else {rv36<<- paste0("     2.  ",as.character(fii36[i,9])," right ventricular size and systolic function\n")}
  lv60<<- paste0("     1.  ",as.character(fii60[i,6]))
  val60<<- as.character(fii60[i,7])
  if (val60=="Normal"){val60<<-"     2.  No significant"} else {val60<<-"     2.  Significant"}
  lv7yr<<- paste0("     1.  ",as.character(fii7yr[i,6]))
  val7yr<<- as.character(fii7yr[i,7])
  if (val7yr=="Normal"){val7yr<<-"     2.  No significant"} else {val7yr<<-"     2.  Significant"}
  
  visit_depress<<- tm7yr_data["visit_depress"]
  if (visit_depress == 1) {
    gds_phys<<- paste0("On a measure assessing depressive symptoms, ",first_name," scored in a range suggesting mild/moderate/severe symptoms of depression. Based upon this score, we recommended that ",first_name," make an appointment for a more detailed clinical assessment of these symptoms.")
    gds<<- paste0("As discussed on ",feedback_date1_7yr,", your scores on a measure assessing depressive symptoms fell in a range suggesting mild/moderate/severe symptoms of depression.  We recommend you make an appointment for a more detailed clinical assessment of these symptoms.  You can request a referral from your primary care doctor.  We would recommend our colleagues who offer clinical services in the Department of Psychiatry at Vanderbilt University.  You can schedule an appointment by calling: 615-936-3555.")
  } else {gds<<- ""; gds_phys<<- ""}
  
  if (any(countss < 1.5)) {
    decline_phys<<- paste0("Based upon the cognitive scores, we suggested that ",first_name," follow-up with a clinical memory work-up for more detailed memory testing. This evaluation can be completed by our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center, with a doctor\'s referral (615-936-0060). ")
    decline<<- paste0("As we discussed on ",feedback_date1_7yr,", we recommend that you make an appointment for a clinical memory workup for more detailed cognitive testing. You can request a referral from your primary care doctor. We would recommend our colleagues in the Cognitive & Behavioral Neurology Division at Vanderbilt University Medical Center. With a doctor's referral, you can schedule an appointment by calling: 615-936-0060.")
  } else {decline<<- ""; decline_phys<<- ""}
  
  if (fii36["extracardiac_incidental"]=="Yes") {lung36<<-paste("     4.  ",fii36["extracardiac_incidental_describe"])} else {lung36<<-""}
  if (fii60["extracardiac_incidental"]=="Yes") {lung60<<-paste("     3.  ",fii60["extracardiac_incidental_describe"])} else {lung60<<-""}
  if (fii7yr["extracardiac_incidental"]=="Yes") {lung7yr<<-paste("     3.  ",fii7yr["extracardiac_incidental_describe"])} else {lung7yr<<-""}
  
  if (fii36["brain_incidental"]=="Yes" | fii60["brain_incidental"]=="Yes" | fii7yr["brain_incidental"]=="Yes") {
    brain_intro1<<-"Brain Test Results"
    brain_intro2<<-"You underwent brain testing, which was read by board-certified neuroradiologists."
  } else {brain_intro1<<-""; brain_intro2<<-""}
  
  if (fii36["brain_incidental"]=="Yes") {brain_intro36<<-paste0("Your _ results on ",fu_date_36mos," were as follows:"); brain36<<-fii36["brain_incidental_davis"]} else {brain_intro36<<-""; brain36<<-""}
  
  if (fii60["brain_incidental"]=="Yes") {brain_intro60<<-paste0("Your _ results on ",fu_date_60mos," were as follows:"); brain60<<-fii60["brain_incidental_davis"]} else {brain_intro60<<-""; brain60<<-""}
  
  if (fii7yr["brain_incidental"]=="Yes") {brain_intro7yr<<-paste0("Your _ results on ",fu_date_7yr," were as follows:"); brain7yr<<-fii7yr["brain_incidental_davis"]} else {brain_intro7yr<<-""; brain7yr<<-""}
  
  if (nchar(map_id)==1) {input<<- paste0("00",map_id)} else if (nchar(map_id)==2) {input<<- paste0("0",map_id)} else {input<<- map_id}
  vmac_id<<- as.character(pdb_data[i,"vmac_id"])
  if (nchar(vmac_id)==1) {record<<- paste0("0000",vmac_id)} else if (nchar(vmac_id)==2) {record<<- paste0("000",vmac_id)} else if (nchar(vmac_id)==3) {record<<- paste0("00",vmac_id)} else if (nchar(vmac_id)==4) {record<<- paste0("0",vmac_id)} else {record<<- vmac_id}
  output<- paste0("C:/Users/sweelyb/Documents/resources/output/feedback_letter_MAP_",input,"_",epoch,".docx")
  renderInlineCode(ptp_temp, output)
  
  importFiles(rcon = pdb, file = output, record = record, field = "feedback_letter_7yr",
              overwrite = TRUE, repeat_instance = 1)
  print("Imported File")
  
  if (is.na(first_name_physician1)==FALSE) {
    first_name_physician<<-first_name_physician1
    last_name_physician<<- last_name_physician1
    credentials<<- credentials1
    street_address_physician<<- street_address_physician1
    city_physician<<- city_physician1
    state_physician<<- state_physician1
    zip_physician<<- zip_physician1
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician_letter",
                overwrite = TRUE, repeat_instance = 1)
  }
  if (is.na(first_name_physician2)==FALSE) {
    first_name_physician<<-first_name_physician2
    last_name_physician<<- last_name_physician2
    credentials<<- credentials2
    street_address_physician<<- street_address_physician2
    city_physician<<- city_physician2
    state_physician<<- state_physician2
    zip_physician<<- zip_physician2
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician2_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician2_letter",
                overwrite = TRUE, repeat_instance = 1)
  }
  if (is.na(first_name_physician3)==FALSE) {
    first_name_physician<<-first_name_physician3
    last_name_physician<<- last_name_physician3
    credentials<<- credentials3
    street_address_physician<<- street_address_physician3
    city_physician<<- city_physician3
    state_physician<<- state_physician3
    zip_physician<<- zip_physician3
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician3_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician3_letter",
                overwrite = TRUE, repeat_instance = 1)
  }
  if (is.na(first_name_physician4)==FALSE) {
    first_name_physician<<-first_name_physician4
    last_name_physician<<- last_name_physician4
    credentials<<- credentials4
    street_address_physician<<- street_address_physician4
    city_physician<<- city_physician4
    state_physician<<- state_physician4
    zip_physician<<- zip_physician4
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician4_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician4_letter",
                overwrite = TRUE, repeat_instance = 1)
  }
  if (is.na(first_name_physician5)==FALSE) {
    first_name_physician<<-first_name_physician5
    last_name_physician<<- last_name_physician5
    credentials<<- credentials5
    street_address_physician<<- street_address_physician5
    city_physician<<- city_physician5
    state_physician<<- state_physician5
    zip_physician<<- zip_physician5
    
    output<- paste0("C:/Users/sweelyb/Documents/resources/output/physician5_letter_MAP_",input,"_",epoch,".docx")
    renderInlineCode(phys_temp, output)
    
    importFiles(rcon = pdb, file = output, record = record, field = "feedback_physician5_letter",
                overwrite = TRUE, repeat_instance = 1)
  }
      
      
    #}
  #}
}
