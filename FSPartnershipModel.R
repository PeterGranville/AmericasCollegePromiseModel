library(data.table)
library(dplyr)
library(ggplot2)
setwd("C:/Users/19732/Desktop/IPEDS/Files") # This folder contains IPEDS data files and other necessary files. 

runCostModel <- function(stateFileName, tribeFileName, geoFileName, definingPredominant){
  
  #########################################################
  #### Step 1 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### First, we load in basic institutional           ####
  #### characteristics and remove all but the public   ####
  #### colleges and the tribal colleges. We also remove####
  #### system offices and institutions that do not     ####
  #### grant degrees. This data set is the base for    #### 
  #### the analysis.                                   ####
  #########################################################
  
  hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "INSTNM",    # Institution (entity) name
    "CONTROL",   # Control of institution
    "SECTOR",    # Sector of institution 
    "C18BASIC",  # Carnegie Classification (Basic)
    "TRIBAL",    # Tribal college
    "DEGGRANT",  # Degree-granting status
    "HDEGOFR1",  # Highest degree offered
    "STABBR"     # State abbreviation
  ))
  
  hd2020$STABBR2 <- ifelse(hd2020$TRIBAL==1, "Tribal Colleges", hd2020$STABBR) 
  hd2020 <- hd2020 %>% filter(CONTROL==1 | TRIBAL==1) 
  hd2020 <- hd2020 %>% filter(SECTOR>0)
  hd2020 <- hd2020 %>% filter(DEGGRANT==1)
  
  #########################################################
  #### Next, we load in headcount enrollment by race.  ####
  #### We filter for undergraduates due to ACP         ####
  #### eligibility rules. We use 2019 data to fill in  ####
  #### data for institutions with headcount enrollment ####
  #### missing from the 2020 set.                      ####
  #########################################################
  
  effy2020 <- fread("effy2020.csv", header=TRUE, select=c(
    "UNITID",   # Unique identification number of the institution
    "EFFYALEV", # Level and degree/certificate-seeking status of student
    "EFYTOTLT", # Grand total
    "EFYAIANT", # American Indian or Alaska Native total
    "EFYASIAT", # Asian total
    "EFYBKAAT", # Black or African American total
    "EFYHISPT", # Hispanic or Latino total
    "EFYNHPIT", # Native Hawaiian or Other Pacific Islander total
    "EFYWHITT", # White total
    "EFY2MORT", # Two or more races total
    "EFYUNKNT", # Race/ethnicity unknown total
    "EFYNRALT"  # Nonresident alien total
  ))
  effy2020 <- effy2020 %>% filter(EFFYALEV == 2) # All students, Undergraduate total
  names(effy2020) <- c("UNITID", "EFFYALEV20", "EFYTOTLT20", "EFYAIANT20", "EFYASIAT20", "EFYBKAAT20", "EFYHISPT20", "EFYNHPIT20", "EFYWHITT20", "EFY2MORT20", "EFYUNKNT20", "EFYNRALT20")
  
  effy2019 <- fread("effy2019_rv.csv", header=TRUE, select=c(
    "UNITID",   # Unique identification number of the institution
    "EFFYLEV",  # Level and degree/certificate-seeking status of student
    "EFYTOTLT", # Grand total
    "EFYAIANT", # American Indian or Alaska Native total
    "EFYASIAT", # Asian total
    "EFYBKAAT", # Black or African American total
    "EFYHISPT", # Hispanic or Latino total
    "EFYNHPIT", # Native Hawaiian or Other Pacific Islander total
    "EFYWHITT", # White total
    "EFY2MORT", # Two or more races total
    "EFYUNKNT", # Race/ethnicity unknown total
    "EFYNRALT"  # Nonresident alien total
  ))
  effy2019 <- effy2019 %>% filter(EFFYLEV == 2) # All students, Undergraduate total
  names(effy2019) <- c("UNITID", "EFFYALEV19", "EFYTOTLT19", "EFYAIANT19", "EFYASIAT19", "EFYBKAAT19", "EFYHISPT19", "EFYNHPIT19", "EFYWHITT19", "EFY2MORT19", "EFYUNKNT19", "EFYNRALT19")
  
  effy <- full_join(x=effy2020, y=effy2019, by="UNITID")
  effy$EFYTOTLT <- ifelse(is.na(effy$EFYTOTLT20), effy$EFYTOTLT19, effy$EFYTOTLT20)
  effy$EFYAIANT <- ifelse(is.na(effy$EFYAIANT20), effy$EFYAIANT19, effy$EFYAIANT20)
  effy$EFYASIAT <- ifelse(is.na(effy$EFYASIAT20), effy$EFYASIAT19, effy$EFYASIAT20)
  effy$EFYBKAAT <- ifelse(is.na(effy$EFYBKAAT20), effy$EFYBKAAT19, effy$EFYBKAAT20)
  effy$EFYHISPT <- ifelse(is.na(effy$EFYHISPT20), effy$EFYHISPT19, effy$EFYHISPT20)
  effy$EFYNHPIT <- ifelse(is.na(effy$EFYNHPIT20), effy$EFYNHPIT19, effy$EFYNHPIT20)
  effy$EFYWHITT <- ifelse(is.na(effy$EFYWHITT20), effy$EFYWHITT19, effy$EFYWHITT20)
  effy$EFY2MORT <- ifelse(is.na(effy$EFY2MORT20), effy$EFY2MORT19, effy$EFY2MORT20)
  effy$EFYUNKNT <- ifelse(is.na(effy$EFYUNKNT20), effy$EFYUNKNT19, effy$EFYUNKNT20)
  effy$EFYNRALT <- ifelse(is.na(effy$EFYNRALT20), effy$EFYNRALT19, effy$EFYNRALT20)
  
  effy <- effy %>% select(UNITID, EFYTOTLT, EFYAIANT, EFYASIAT, EFYBKAAT, EFYHISPT, EFYNHPIT, EFYWHITT, EFY2MORT, EFYUNKNT, EFYNRALT)
  
  fullData <- left_join(x=hd2020, y=effy, by="UNITID")

  fullData$EFYTOTLT <- as.numeric(fullData$EFYTOTLT)
  fullData$EFYAIANT <- as.numeric(fullData$EFYAIANT)
  fullData$EFYASIAT <- as.numeric(fullData$EFYASIAT)
  fullData$EFYBKAAT <- as.numeric(fullData$EFYBKAAT)
  fullData$EFYHISPT <- as.numeric(fullData$EFYHISPT)
  fullData$EFYNHPIT <- as.numeric(fullData$EFYNHPIT)
  fullData$EFYWHITT <- as.numeric(fullData$EFYWHITT)
  fullData$EFY2MORT <- as.numeric(fullData$EFY2MORT)
  fullData$EFYUNKNT <- as.numeric(fullData$EFYUNKNT)
  fullData$EFYNRALT <- as.numeric(fullData$EFYNRALT)
  
  #########################################################
  #### We also need to pull in some data from finance  ####
  #### files for reference later on.                   ####
  #########################################################
  
  f1819_f1a <- fread("f1819_f1a.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "F1E03",     # Grants by state government
    "F1B01",     # Tuition and fees, after deducting discounts and allowances
    "F1E08"      # Discounts and allowances applied to tuition and fees
  ))
  f1819_f1a$F1E08[is.na(f1819_f1a$F1E08)] <- 0   # All institutions with NAs are system offices
  f1819_f1a$tuitionAndFeeRevenue <- f1819_f1a$F1B01 + f1819_f1a$F1E08
  f1819_f1a <- f1819_f1a %>% select(UNITID, F1E03, tuitionAndFeeRevenue)
  
  f1819_f2 <- fread("f1819_f2.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "F2C03",     # State grants 
    "F2D01",     # Tuition and fees - Total
    "F2C08"      # Allowances applied to tuition and fees
  ))
  f1819_f2$F2C08[is.na(f1819_f2$F2C08)] <- 0      # All institutions with NAs are system offices
  f1819_f2$tuitionAndFeeRevenue <- f1819_f2$F2D01 + f1819_f2$F2C08
  f1819_f2 <- f1819_f2 %>% select(UNITID, F2C03, tuitionAndFeeRevenue)
  
  names(f1819_f1a) <- c("UNITID", "STATEGRANTS", "tuitionAndFeeRevenue")
  names(f1819_f2) <- c("UNITID", "STATEGRANTS", "tuitionAndFeeRevenue")
  
  f1819_f1a$STATEGRANTS[is.na(f1819_f1a$STATEGRANTS)] <- 0 # All institutions with NAs are system offices
  f1819_f2$STATEGRANTS[is.na(f1819_f2$STATEGRANTS)] <- 0   # All institutions with NAs are system offices
  
  f1819 <- rbind(f1819_f1a, f1819_f2)
  fullData <- left_join(x=fullData, y=f1819, by="UNITID")
  
  #########################################################
  #### Step 2 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Next, we load in tuition and fees charges and   ####
  #### use a left-join to merge it to our base set.    ####
  #########################################################
  
  ic2020_ay <- fread("ic2020_ay.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "TUITION1",  # In-district average tuition for full-time undergraduates, 2020-21 
    "FEE1",      # In-district required fees for full-time undergraduates, 2020-21
    "TUITION2",  # In-state average tuition for full-time undergraduates, 2020-21 
    "FEE2",      # In-state required fees for full-time undergraduates, 2020-21
    "TUITION3",  # Out-of-state average tuition for full-time undergraduates
    "FEE3"       # Out-of-state required fees for full-time undergraduates
  ))

  ic2020_ay$TUITION1 <- ifelse(ic2020_ay$TUITION1 == ".", NA, ic2020_ay$TUITION1)
  ic2020_ay$FEE1 <- ifelse(ic2020_ay$FEE1 == ".", NA, ic2020_ay$FEE1)
  ic2020_ay$TUITION2 <- ifelse(ic2020_ay$TUITION2 == ".", NA, ic2020_ay$TUITION2)
  ic2020_ay$FEE2 <- ifelse(ic2020_ay$FEE2 == ".", NA, ic2020_ay$FEE2)
  ic2020_ay$TUITION3 <- ifelse(ic2020_ay$TUITION3 == ".", NA, ic2020_ay$TUITION3)
  ic2020_ay$FEE3 <- ifelse(ic2020_ay$FEE3 == ".", NA, ic2020_ay$FEE3)

  ic2020_ay$TUITION1 <- as.numeric(ic2020_ay$TUITION1)
  ic2020_ay$FEE1 <- as.numeric(ic2020_ay$FEE1)
  ic2020_ay$TUITION2 <- as.numeric(ic2020_ay$TUITION2)
  ic2020_ay$FEE2 <- as.numeric(ic2020_ay$FEE2)
  ic2020_ay$TUITION3 <- as.numeric(ic2020_ay$TUITION3)
  ic2020_ay$FEE3 <- as.numeric(ic2020_ay$FEE3)
  
  ic2020_ay$differentPricing <- ifelse((ic2020_ay$TUITION1 != ic2020_ay$TUITION2) | (ic2020_ay$TUITION3 != ic2020_ay$TUITION2), "Yes", "No")
  
  ic2020_ay$inStateTuitionAndFees <- ic2020_ay$TUITION2 + ic2020_ay$FEE2 
  
  fullData <- left_join(x=fullData, y=ic2020_ay, by="UNITID")

  #########################################################
  #### Step 3 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Next we define what counts as a community       ####
  #### college. According to the bill text, it is any  ####
  #### public institution where the highest degree     ####
  #### awarded is the associate degree or where the    ####
  #### predominant degree awarded is the associate     ####
  #### degree. We use awards from 2017-18, 2018-19,    ####
  #### and 2019-20 to make these determinations.       ####
  #########################################################
  
  c2020_a <- fread("c2020_a.csv", header=TRUE, select=c(
    "UNITID", 
    "CIPCODE", 
    "MAJORNUM", 
    "AWLEVEL", 
    "CTOTALT"
  ))
  c2020_a <- c2020_a %>% filter(CIPCODE==99)
  c2020_a <- c2020_a %>% filter(MAJORNUM==1)
  c2020_a <- c2020_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))
  
  c2019_a <- fread("c2019_a.csv", header=TRUE, select=c(
    "UNITID", 
    "CIPCODE", 
    "MAJORNUM", 
    "AWLEVEL", 
    "CTOTALT"
  ))
  c2019_a <- c2019_a %>% filter(CIPCODE==99)
  c2019_a <- c2019_a %>% filter(MAJORNUM==1)
  c2019_a <- c2019_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))
  
  c2018_a <- fread("c2018_a_rv.csv", header=TRUE, select=c(
    "UNITID", 
    "CIPCODE", 
    "MAJORNUM", 
    "AWLEVEL", 
    "CTOTALT"
  ))
  c2018_a <- c2018_a %>% filter(CIPCODE==99)
  c2018_a <- c2018_a %>% filter(MAJORNUM==1)
  c2018_a <- c2018_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))
  
  c20xx_a <- rbind(c2020_a, c2019_a, c2018_a)
  
  if(definingPredominant==TRUE){
    tbl1All <- aggregate(data=c20xx_a, CTOTALT ~ UNITID, FUN=sum)
    names(tbl1All) <- c("UNITID", "AllDegrees")
    associateDegreesOnly <- c20xx_a %>% filter(AWLEVEL==3)
    tbl1Assoc <- aggregate(data=associateDegreesOnly, CTOTALT ~ UNITID, FUN=sum)
    names(tbl1Assoc) <- c("UNITID", "AssociateDegrees")
    fullData <- left_join(x=fullData, y=tbl1All, by="UNITID")
    fullData <- left_join(x=fullData, y=tbl1Assoc, by="UNITID")
    fullData$AssociateDegrees <- ifelse(is.na(fullData$AssociateDegrees), 0, fullData$AssociateDegrees)
    fullData$associateShare <- fullData$AssociateDegrees / fullData$AllDegrees
    fullData <- fullData %>% filter(HDEGOFR1==40 | associateShare >= 0.5)
  }
  if(definingPredominant==FALSE){
    tbl1 <- dcast(data=c20xx_a, UNITID ~ AWLEVEL, value.var = "CTOTALT", fun.aggregate=sum)
    names(tbl1) <- c("UNITID", "Assoc", "Bach", "Mast", "Doc1", "Doc2", "Doc3")
    tbl1 <- tbl1 %>% filter(Assoc + Bach + Mast + Doc1 + Doc2 + Doc3 > 0)
    tbl1$highest <- colnames(tbl1[, 2:7])[max.col(tbl1[, 2:7], ties.method = "first")]
    tbl2 <- tbl1 %>% select(UNITID, highest)
    fullData <- left_join(x=fullData, y=tbl2, by="UNITID")
    fullData <- fullData %>% filter(HDEGOFR1==40 | highest=="Assoc")
  }
  
  #########################################################
  #### Step 4 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Next, we must address missing tuition and fees  ####
  #### for some institutions. We use College Scorecard ####
  #### tuition revenue per FTE as a proxy. This is     ####
  #### only necessary for 15 institutions in our set.  ####
  #########################################################
  
  collegeScorecard <- fread("Most-Recent-Cohorts-All-Data-Elements.csv", header=TRUE, select=c(
    "UNITID", 
    "TUITFTE"
  ))
  fullData$inStateTuitionAndFees[fullData$UNITID == 155618] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==155618]  # Northwest Kansas Technical College (KS)
  fullData$inStateTuitionAndFees[fullData$UNITID == 369668] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==369668]  # Central Pennsylvania Institute of Science and Technology (PA)
  fullData$inStateTuitionAndFees[fullData$UNITID == 383084] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==383084]  # Hacienda La Puente Adult Education (CA)
  fullData$inStateTuitionAndFees[fullData$UNITID == 413802] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==413802]  # East San Gabriel Valley Regional Occupational Program (CA)
  fullData$inStateTuitionAndFees[fullData$UNITID == 418533] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==418533]  # Lancaster County Career and Technology Center (PA)
  fullData$inStateTuitionAndFees[fullData$UNITID == 430795] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==430795]  # Carver Career Center (WV)
  fullData$inStateTuitionAndFees[fullData$UNITID == 491844] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==491844]  # Red Lake Nation College
  fullData$inStateTuitionAndFees[fullData$UNITID == 214740] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==214740] # Pennsylvania State University-Penn State DuBois
  fullData$inStateTuitionAndFees[fullData$UNITID == 223524] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==223524] # Brookhaven College
  fullData$inStateTuitionAndFees[fullData$UNITID == 223773] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==223773] # Cedar Valley College
  fullData$inStateTuitionAndFees[fullData$UNITID == 224572] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==224572] # Eastfield College
  fullData$inStateTuitionAndFees[fullData$UNITID == 226930] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==226930] # Mountain View College
  fullData$inStateTuitionAndFees[fullData$UNITID == 227191] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==227191] # North Lake College
  fullData$inStateTuitionAndFees[fullData$UNITID == 227766] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==227766] # Richland College
  fullData$inStateTuitionAndFees[fullData$UNITID == 439145] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==439145] # Pierce College-Puyallup
  fullData$inStateTuitionAndFees[fullData$UNITID == 214795] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
  
  fullData$inStateTuitionAndFees <- as.numeric(fullData$inStateTuitionAndFees)
  
  #########################################################
  #### Step 5 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### We now have tuition and fee revenue for every   ####
  #### community college and tribal college. We next   ####
  #### load in FTE enrollment data.                    ####
  #########################################################
  
  efia2020 <- fread("efia2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "EFTEUG"     # Estimated full-time equivalent (FTE) undergraduate enrollment, 2018-19
  ))
  fullData <- left_join(x=fullData, y=efia2020, by="UNITID")
  
  #########################################################
  #### For some institutions with missing data, we     ####
  #### pull in data from the previous year.            ####
  #########################################################
  
  efia2019 <- fread("efia2019.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "EFTEUG"     # Estimated full-time equivalent (FTE) undergraduate enrollment, 2017-18
  ))
  
  fullData$EFTEUG[fullData$UNITID==214740] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==214740]) # Pennsylvania State University-Penn State DuBois
  fullData$EFTEUG[fullData$UNITID==223524] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==223524]) # Brookhaven College
  fullData$EFTEUG[fullData$UNITID==223773] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==223773]) # Cedar Valley College
  fullData$EFTEUG[fullData$UNITID==224572] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==224572]) # Eastfield College
  fullData$EFTEUG[fullData$UNITID==226930] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==226930]) # Mountain View College
  fullData$EFTEUG[fullData$UNITID==227191] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==227191]) # North Lake College
  fullData$EFTEUG[fullData$UNITID==227766] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==227766]) # Richland College
  fullData$EFTEUG[fullData$UNITID==413802] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==413802]) # East San Gabriel Valley Regional Occupational Program
  fullData$EFTEUG[fullData$UNITID==439145] <- as.numeric(efia2019$EFTEUG[efia2019$UNITID==439145]) # Pierce College-Puyallup

  fullData$EFTEUG <- as.numeric(fullData$EFTEUG)
  
  #########################################################
  #### Now we fill in some institutions' missing total ####
  #### tuition revenue, using a combination of IPEDS's ####
  #### undergrad FTE enrollment totals and the tuition ####
  #### revenue per FTE in College Scorecard.           ####
  #########################################################
  
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 125499] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 125499]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 125499]) # West Valley College (CA)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 180081] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 180081]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 180081]) # Highlands College of Montana Tech (MT)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 200846] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 200846]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 200846]) # University of Akron Wayne College (OH)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 201432] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 201432]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 201432]) # Bowling Green State University-Firelands (OH)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 214740] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 214740]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 214740]) # Pennsylvania State University-Penn State DuBois (PA)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 237701] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 237701]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 237701]) # Potomac State College of West Virginia University (WV)
  fullData$tuitionAndFeeRevenue[fullData$UNITID == 382911] <- as.numeric(collegeScorecard$TUITFTE[collegeScorecard$UNITID == 382911]) * as.numeric(fullData$EFTEUG[fullData$UNITID == 382911]) # Southwest Collegiate Institute for the Deaf (TX)
  
  #########################################################
  #### Step 6 of methodology:                          ####
  #########################################################
  
  #########################################################
  ####            Method 1 for in-state PCT            ####
  #########################################################
  #### Now, we must account for who is enrolling       ####
  #### in their own state. (We treat tribal colleges   ####
  #### like others for now using STABBR but we will    ####
  #### later account for their special treatment under ####
  #### the bill using STABBR2.) We derive for each     ####
  #### institution the percentage of headcount enroll- ####
  #### ment that is in-state and will then multiply    ####
  #### institution-level FTE by that number.           ####
  #########################################################
  
  ef2019c <- fread("ef2019c.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "EFCSTATE",  # State of residence when student was first admitted: Fall 2019
    "EFRES01"    # First-time degree/certificate-seeking undergraduate students: Fall 2019
  ))
  anotherhd2020 <- fread("hd2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "STABBR"     # State abbreviation
  ))
  statesDF <- read.csv("statesForEF2019C.csv", header=TRUE)
  states <- left_join(x=ef2019c, y=anotherhd2020, by="UNITID")
  states <- left_join(x=states, y=statesDF, by="EFCSTATE")
  states$inStateEnrollment <- ifelse(states$STABBR == states$BriefStateName, states$EFRES01, 0)
  states$totalEnrollment <- ifelse(states$BriefStateName == "ALLTOT", states$EFRES01, 0)
  inStateEnrollment <- aggregate(data=states, inStateEnrollment ~ UNITID, FUN=sum)
  totalEnrollment <- aggregate(data=states, totalEnrollment ~ UNITID, FUN=sum)
  enrollmentCalcs <- inner_join(x=inStateEnrollment, y=totalEnrollment, by="UNITID")
  enrollmentCalcs$inStatePct <- enrollmentCalcs$inStateEnrollment / enrollmentCalcs$totalEnrollment
  fullData <- left_join(x=fullData, y=enrollmentCalcs, by="UNITID")
  
  #########################################################
  ####            Method 2 for in-state PCT            ####
  #########################################################
  #### Okay, so we have 308 institutions that lack     ####
  #### the data we need for the methods above. Now, we ####
  #### use another approach, looking at the percentage ####
  #### of each institution's financial aid cohort that ####
  #### is paying in-district or in-state tuition.      ####
  #########################################################
  
  sfa1819 <- fread("sfa1819.csv", header=TRUE, select=c(
    "UNITID",   # Unique identification number of the institution
    "SCFA11N",  # Number of students in fall cohort who are paying in-district tuition rates, 2018-19
    "SCFA12N",  # Number of students in fall cohort who are paying in-state tuition rates, 2018-19
    "SCFA13N",  # Number of students in fall cohort who are paying out-of-state tuition rates, 2018-19
    "SCFA11P",  # Percentage of students in fall cohort who are paying in-district tuition rates, 2018-19
    "SCFA12P",  # Percentage of students in fall cohort who are paying in-state tuition rates, 2018-19
    "SCFA13P",  # Percentage of students in fall cohort who are paying out-of-state tuition rates, 2018-19
    "SCFA14P"   # Percentage of students in fall cohort whose residence/ tuition rate is unknown
  ))
  sfa1819$SCFA11N[is.na(sfa1819$SCFA11N)] <- 0
  sfa1819$SCFA12N[is.na(sfa1819$SCFA12N)] <- 0
  sfa1819$SCFA13N[is.na(sfa1819$SCFA13N)] <- 0
  sfa1819$SCFA11N <- as.numeric(sfa1819$SCFA11N)
  sfa1819$SCFA12N <- as.numeric(sfa1819$SCFA12N)
  sfa1819$SCFA13N <- as.numeric(sfa1819$SCFA13N)
  sfa1819$residentPct <- (sfa1819$SCFA11N + sfa1819$SCFA12N) / (sfa1819$SCFA11N + sfa1819$SCFA12N + sfa1819$SCFA13N)
  sfa1819$residentPct <- as.numeric(sfa1819$residentPct)
  sfa1819$residentPct[is.nan(sfa1819$residentPct)] <- NA
  fullData <- left_join(x=fullData, y=sfa1819, by="UNITID")
  
  # For Discussion Item 2 in the methodology: 
  # sum(is.na(fullData$residentPct) & is.na(fullData$inStatePct))
  # sum(is.na(fullData$residentPct) | is.na(fullData$inStatePct))
  # sum(is.na(fullData$residentPct)==FALSE & is.na(fullData$inStatePct)==FALSE)
  
  # Inspecting the relationship between these two in-state enrollment measures 
  # ggplot(data=fullData, mapping=aes(x=residentPct, y=inStatePct))+geom_point()
  # cor(x=fullData$residentPct, y=fullData$inStatePct, use="complete.obs")
  
  #########################################################
  #### Given these two methods of estimating in-state  ####
  #### enrollment percentage, we take the average of   ####
  #### the two for each institution with complete data ####
  #### and, where an institution is missing data for   ####
  #### either, the other data point is used. For those ####
  #### with neither data point, a value of 1 is imputed####
  #### to err on the side of overestimating costs.     ####
  #########################################################
  
  fullData$theoreticalResidentPct <- ifelse(is.na(fullData$residentPct)==FALSE, 
                             ifelse(is.na(fullData$inStatePct)==FALSE, 
                                    ((fullData$residentPct + fullData$inStatePct)*0.5), 
                                    fullData$residentPct), 
                              ifelse(is.na(fullData$inStatePct)==FALSE, 
                                     fullData$inStatePct, 1))
  
  #########################################################
  #### The next line is to account for the bill text's ####
  #### handling of students at institutions that do    ####
  #### not charge different rates based on in-state or ####
  #### in-district residency.                          ####
  #########################################################
  
  fullData$newResidentPct <- ifelse(fullData$differentPricing == "Yes", fullData$theoreticalResidentPct, 1)
  
  #########################################################
  #### Now we just apply this residency percentage to  ####
  #### all our enrollment variables.                   ####
  #########################################################
  
  fullData$residentFTE <- fullData$newResidentPct * fullData$EFTEUG
  
  fullData$EFYTOTLT <- fullData$EFYTOTLT * fullData$newResidentPct
  fullData$EFYAIANT <- fullData$EFYAIANT * fullData$newResidentPct
  fullData$EFYASIAT <- fullData$EFYASIAT * fullData$newResidentPct
  fullData$EFYBKAAT <- fullData$EFYBKAAT * fullData$newResidentPct
  fullData$EFYHISPT <- fullData$EFYHISPT * fullData$newResidentPct
  fullData$EFYNHPIT <- fullData$EFYNHPIT * fullData$newResidentPct
  fullData$EFYWHITT <- fullData$EFYWHITT * fullData$newResidentPct
  fullData$EFY2MORT <- fullData$EFY2MORT * fullData$newResidentPct
  fullData$EFYUNKNT <- fullData$EFYUNKNT * fullData$newResidentPct
  fullData$EFYNRALT <- fullData$EFYNRALT * fullData$newResidentPct
  
  #########################################################
  #### Step 7 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Next, we must account for less-than-            ####
  #### half-time enrollment. See full methodology for  ####
  #### the explanation, but we multiply FTEs by a fixed####
  #### coefficient, and we multiply headcount          ####
  #### enrollment by another (but different)           ####
  #### coefficient.                                    ####
  #########################################################
  
  fullData$adjResidentFTE <- fullData$residentFTE * 0.9459854
  
  fullData$EFYTOTLT <- fullData$EFYTOTLT * 0.852
  fullData$EFYAIANT <- fullData$EFYAIANT * 0.852
  fullData$EFYASIAT <- fullData$EFYASIAT * 0.852
  fullData$EFYBKAAT <- fullData$EFYBKAAT * 0.852
  fullData$EFYHISPT <- fullData$EFYHISPT * 0.852
  fullData$EFYNHPIT <- fullData$EFYNHPIT * 0.852
  fullData$EFYWHITT <- fullData$EFYWHITT * 0.852
  fullData$EFY2MORT <- fullData$EFY2MORT * 0.852
  fullData$EFYUNKNT <- fullData$EFYUNKNT * 0.852
  fullData$EFYNRALT <- fullData$EFYNRALT * 0.852
  
  #########################################################
  #### Step 8 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Before we move from institution-level data      ####
  #### to a state-level model, we need to add in info  ####
  #### about Title III MSIs.                           ####
  #########################################################
  
  collegeScorecard2 <- fread("Most-Recent-Cohorts-All-Data-Elements.csv", header=TRUE, select=c(
    "UNITID",   # Unit ID for institution
    "HBCU",     # Flag for Historically Black College and University
    "PBI",      # Flag for predominantly black institution
    "ANNHI",    # Flag for Alaska Native Native Hawaiian serving institution
    "AANAPII",  # Flag for Asian American Native American Pacific Islander-serving institution
    "HSI",      # Flag for Hispanic-serving institution
    "NANTI"     # Flag for Native American non-tribal institution
  ))
  fullData <- left_join(x=fullData, y=collegeScorecard2, by="UNITID")
  
  AANAPIIbyState <- aggregate(data=fullData[fullData$AANAPII==1], adjResidentFTE ~ STABBR2, FUN=sum)
  ANNHbyState <- aggregate(data=fullData[fullData$ANNH==1], adjResidentFTE ~ STABBR2, FUN=sum)
  HBCUbyState <- aggregate(data=fullData[fullData$HBCU==1], adjResidentFTE ~ STABBR2, FUN=sum)
  HSIbyState <- aggregate(data=fullData[fullData$HSI==1], adjResidentFTE ~ STABBR2, FUN=sum)
  NANTIbyState <- aggregate(data=fullData[fullData$NANTI==1], adjResidentFTE ~ STABBR2, FUN=sum)
  PBIbyState <- aggregate(data=fullData[fullData$PBI==1], adjResidentFTE ~ STABBR2, FUN=sum)
  TCUbyState <- aggregate(data=fullData[fullData$TRIBAL==1], adjResidentFTE ~ STABBR2, FUN=sum)
  
  names(AANAPIIbyState) <- c("STABBR2", "AANAPIIfte")
  names(ANNHbyState) <- c("STABBR2", "ANNHfte")
  names(HBCUbyState) <- c("STABBR2", "HBCUfte")
  names(HSIbyState) <- c("STABBR2", "HSIfte")
  names(NANTIbyState) <- c("STABBR2", "NANTIfte")
  names(PBIbyState) <- c("STABBR2", "PBIfte")
  names(TCUbyState) <- c("STABBR2", "TCUfte")
  
  MSIbyState <- full_join(x=AANAPIIbyState, y=
                  full_join(x=ANNHbyState, y=
                    full_join(x=HBCUbyState, y=
                        full_join(x=HSIbyState, y=
                            full_join(x=NANTIbyState, y=
                                full_join(x=PBIbyState, y=TCUbyState, by="STABBR2"), 
                            by="STABBR2"), 
                        by="STABBR2"), 
                    by="STABBR2"), 
                  by="STABBR2"), 
                by="STABBR2")
  
  #########################################################
  #### We now do a similar process for data on         ####
  #### headcount enrollment by race.                   ####
  #########################################################

  fullData$EFYAAPIT <- fullData$EFYASIAT + fullData$EFYNHPIT
  fullData$EFYTWOTT <- fullData$EFY2MORT + fullData$EFYUNKNT + fullData$EFYNRALT
  
  EFYTOTLTbyState <- aggregate(data=fullData, EFYTOTLT ~ STABBR2, FUN=sum) # Grand total
  EFYAAPITbyState <- aggregate(data=fullData, EFYAAPIT ~ STABBR2, FUN=sum) # Asian / Pacific Islander total 
  EFYAIANTbyState <- aggregate(data=fullData, EFYAIANT ~ STABBR2, FUN=sum) # American Indian or Alaska Native total
  EFYBKAATbyState <- aggregate(data=fullData, EFYBKAAT ~ STABBR2, FUN=sum) # Black or African American total
  EFYHISPTbyState <- aggregate(data=fullData, EFYHISPT ~ STABBR2, FUN=sum) # Hispanic or Latino total
  EFYWHITTbyState <- aggregate(data=fullData, EFYWHITT ~ STABBR2, FUN=sum) # White total 
  EFYTWOTTbyState <- aggregate(data=fullData, EFYTWOTT ~ STABBR2, FUN=sum) # Two or more races / Other total 
  
  EFFYbyState <- full_join(x=EFYTOTLTbyState, y=
                  full_join(x=EFYAAPITbyState, y=
                    full_join(x=EFYAIANTbyState, y=
                      full_join(x=EFYBKAATbyState, y=
                        full_join(x=EFYHISPTbyState, y=
                          full_join(x=EFYWHITTbyState, y=EFYTWOTTbyState, by="STABBR2"), 
                        by="STABBR2"), 
                      by="STABBR2"), 
                    by="STABBR2"), 
                  by="STABBR2"), 
                by="STABBR2")
  
  #########################################################
  #### Step 9 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### We need to find the actual cost of eliminating  ####
  #### tuition and fees. First we find the weighted    ####
  #### tuition charges per institution for resident    ####
  #### students, based on students in the fall cohort. ####
  #### This takes cue from "differentPricing" which    ####
  #### affects who is counted as a resident student.   ####
  #########################################################
  
  fullData$weightedTuitionFees <- ifelse(fullData$differentPricing=="No", (fullData$TUITION2 + fullData$FEE2), (((fullData$TUITION1 + fullData$FEE1) * (fullData$SCFA11N / (fullData$SCFA11N + fullData$SCFA12N))) + ((fullData$TUITION2 + fullData$FEE2) * (fullData$SCFA12N / (fullData$SCFA11N + fullData$SCFA12N)))))

  fullData$tuitionFTEproduct <- fullData$adjResidentFTE * fullData$weightedTuitionFees
  
  #########################################################
  #### Now we prepare to aggregate data by state.      ####
  #########################################################
  
  fullStateData <- fullData %>% filter(TRIBAL==2 & (STABBR2 %in% c("AS",	# American Samoa
                                                                   "DC",  # District of Columbia
                                                                   "FM",	# Federated States of Micronesia
                                                                   "GU",	# Guam
                                                                   "MH",	# Marshall Islands
                                                                   "MP",	# Northern Marianas
                                                                   "PW",	# Palau
                                                                   "PR",	# Puerto Rico
                                                                   "VI"	# Virgin Islands
  )==FALSE))
  
  fullTribeData <- fullData %>% filter(TRIBAL==1 | STABBR2 %in% c("AS",	# American Samoa
                                                                  "DC",  # District of Columbia
                                                                  "FM",	# Federated States of Micronesia
                                                                  "GU",	# Guam
                                                                  "MH",	# Marshall Islands
                                                                  "MP",	# Northern Marianas
                                                                  "PW",	# Palau
                                                                  "PR",	# Puerto Rico
                                                                  "VI"	# Virgin Islands
  ))
  
  #########################################################
  #### Here we record the median tuition/fees amount, a####
  #### key feature of the partnership. We also record  ####
  #### the partnership cost by community college,      ####
  #### which is a function of the median tuition/fees. ####
  #########################################################
  
  medianTuition <- median(fullStateData$inStateTuitionAndFees)
  
  fullStateData$partnershipCost <- fullStateData$adjResidentFTE * medianTuition
  fullTribeData$partnershipCost <- fullTribeData$adjResidentFTE * medianTuition
  
  #########################################################
  #### Here we account for how tribes' federal         ####
  #### allocations are the greater of two possible     ####
  #### values: the cost of 100% of the ACP amount,     ####
  #### and the total cost of eliminating tuition and   ####
  #### fees for eligible students. We also expect the  ####
  #### federal government to pick up the tab for       ####
  #### territories, covering 100% of ACP costs.        ####
  #########################################################
  
  fullTribeData$federalAllocation <- ifelse(fullTribeData$TRIBAL==1, 
                                            ifelse(fullTribeData$partnershipCost >= fullTribeData$tuitionFTEproduct, fullTribeData$partnershipCost, fullTribeData$tuitionFTEproduct), 
                                            fullTribeData$partnershipCost)
 
  #########################################################
  #### We now start aggregating by state/territory/TCU.####
  #########################################################
  
  # Starting with adjResidentFTE (both sets)
  stateModel <- aggregate(data=fullStateData, adjResidentFTE ~ STABBR2, FUN=sum) 
  tribeModel <- aggregate(data=fullTribeData, adjResidentFTE ~ STABBR2, FUN=sum)
  
  # Adding undergraduate FTE enrollment (both sets)
  stateModel <- left_join(x=stateModel, y=
                            aggregate(data=fullStateData, EFTEUG ~ STABBR2, FUN=SUM), 
                          by="STABBR2")
  tribeModel <- left_join(x=tribeModel, y=
                            aggregate(data=fullTribeData, EFTEUG ~ STABBR2, FUN=SUM),
                          by="STABBR2")
  
  # Adding federalAllocation (territories/TCUs)
  tribeModel <- left_join(x=tribeModel, 
                          y=aggregate(data=fullTribeData, federalAllocation ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  
  # Adding partnershipCost (states)
  stateModel <- left_join(x=stateModel, 
                          y=aggregate(data=fullStateData, partnershipCost ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  
  # Adding tuitionFTEproduct (states)
  stateModel <- left_join(x=stateModel, 
                          y=aggregate(data=fullStateData, tuitionFTEproduct ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  
  # Adding MSI data (both sets)
  stateModel <- left_join(x=stateModel, y=MSIbyState, by="STABBR2")
  tribeModel <- left_join(x=tribeModel, y=MSIbyState, by="STABBR2")
  
  # Adding headcount by race (both sets) 
  stateModel <- left_join(x=stateModel, y=EFFYbyState, by="STABBR2")
  tribeModel <- left_join(x=tribeModel, y=EFFYbyState, by="STABBR2")
  
  #########################################################
  #### These lines are for loading in state financial  ####
  #### aid sums by state. Here we use STABBR rather    ####
  #### than STABBR2 because the state would get credit ####
  #### for state financial aid money sent to a tribal  ####
  #### college in its borders.                         ####
  #########################################################
  
  stateFinAid <- aggregate(data=fullData, STATEGRANTS ~ STABBR, FUN=sum) 
  names(stateFinAid) <- c("STABBR2", "stateFinAid")
  stateModel <- left_join(x=stateModel, y=stateFinAid, by="STABBR2")
  
  #########################################################
  #### Let's also add in a counting variable to know   ####
  #### many institutions are included in the           ####
  #### partnership.                                    ####
  #########################################################
  
  fullStateData$countingVar <- rep(1, nrow(fullStateData))
  fullTribeData$countingVar <- rep(1, nrow(fullTribeData))
  
  stateModel <- left_join(x=stateModel, 
                          y=aggregate(data=fullStateData, countingVar ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  tribeModel <- left_join(x=tribeModel, 
                          y=aggregate(data=fullTribeData, countingVar ~ STABBR2, FUN=sum), 
                          by="STABBR2")

  #########################################################
  #### Step 10 of methodology:                          ####
  #########################################################
    
  #########################################################
  #### Now, we must add in the variables that can be   ####
  #### utilized by the user to raise or lower each     ####
  #### state's match rate.                             ####
  #########################################################
  
  stateVars <- fread("Fed-state partnership state variables - AllData.csv", header=TRUE, skip=1, select=c(
    "STABBR2",   # State abbreviation
    "TTRPCindex",    # Total taxable resources per capita: 2016-18
    "CPRindex",      # Child poverty rate: 2015 to 2019
    "PIPCindex",     # Personal income per capita: 2017 to 2019
    "PELLindex",     # Share of public undergrads receiving Pell Grants: AY 2018-19
    "FRPLindex",     # Share of K-12 students eligible for FRPL: AY 2018-19
    "RESwhite",	     # White residents aged 18-35 without a college degree
    "RESblack",      # Black residents aged 18-35 without a college degree
    "REShispanic",	 # Hispanic residents aged 18-35 without a college degree 
    "RESaapi",       # AAPI residents aged 18-35 without a college degree
    "RESnative",	   # Native American / Alaska Native residents aged 18-35 without a college degree
    "REStwo",        # Two or more races / Other race residents aged 18-35 without a college degree
    "RESall",        # Total residents aged 18-35 without a college degree
    "TwoYearEdApprops"  # Two-year educational appropriations 
  ))
  stateModel <- left_join(x=stateModel, y=stateVars, by="STABBR2")
  
  #########################################################
  #### We have what we need for the interactive model, ####
  #### so we now export the state and tribe model sets ####
  #### as csv files.                                   ####
  #########################################################
  
  write.csv(stateModel, stateFileName, row.names=FALSE)
  write.csv(tribeModel, tribeFileName, row.names=FALSE)
  
  #########################################################
  #### We also pull out the latitude, longitude, and   ####
  #### FTEs of community colleges.                     ####
  #########################################################
  
  hd2020_3 <- fread("hd2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "LONGITUD",  # Longitude location of institution
    "LATITUDE"   # Latitude location of institution
  ))
  geoData <- inner_join(x=hd2020_3, y=fullData, by="UNITID")
  geoData <- geoData %>% select(UNITID, INSTNM, adjResidentFTE, LONGITUD, LATITUDE)
  write.csv(geoData, geoFileName, row.names=FALSE)
  
  rm(list = ls())
}

runCostModel("stateModelA.csv", "tribeModelA.csv", "geoFileA.csv", definingPredominant=TRUE)
runCostModel("stateModelB.csv", "tribeModelB.csv", "geoFileB.csv", definingPredominant=FALSE)



### Some code for American Community Survey Public Use Microdata Sample: Data pull on population aged 18-35 without a college degree 
# setwd("C:/Users/19732/Downloads/csv_pus (14)")
# library(data.table)
# library(dplyr)
# pusa <- fread("psam_pusa.csv", header=TRUE, select=c("SERIALNO", "ST", "PWGTP", "AGEP", "RAC1P", "HISP", "SCHL"))
# pusb <- fread("psam_pusb.csv", header=TRUE, select=c("SERIALNO", "ST", "PWGTP", "AGEP", "RAC1P", "HISP", "SCHL"))
# pusc <- fread("psam_pusc.csv", header=TRUE, select=c("SERIALNO", "ST", "PWGTP", "AGEP", "RAC1P", "HISP", "SCHL"))
# pusd <- fread("psam_pusd.csv", header=TRUE, select=c("SERIALNO", "ST", "PWGTP", "AGEP", "RAC1P", "HISP", "SCHL"))
# pusm <- rbind(pusa, pusb, pusc, pusd)
# stateNames <- read.delim("stateNames.txt", sep="\t", header=FALSE)
# names(stateNames) <- c("ST", "State")
# pusm <- left_join(x=pusm, y=stateNames, by="ST")
# raceGroups <- read.delim("forRac1p.txt", sep="\t", header=FALSE)
# names(raceGroups) <- c("RAC1P", "raceGroup")
# pusm <- left_join(x=pusm, y=raceGroups, by="RAC1P")
# pusm$hispanic <- ifelse(pusm$HISP == 1, "Non-Hispanic", "Hispanic")
# aged18to35withoutDegree <- pusm %>% filter(AGEP >= 18 & AGEP <= 35 & SCHL < 20)
# hispanicSet <- aged18to35withoutDegree %>% filter(hispanic=="Hispanic")
# nonHispanicSet <- aged18to35withoutDegree %>% filter(hispanic=="Non-Hispanic")
# tbl1 <- aggregate(data = hispanicSet, PWGTP ~ State, FUN=sum)
# tbl2 <- aggregate(data = nonHispanicSet, PWGTP ~ State + raceGroup, FUN=sum)
# write.csv(tbl1, "hispanicPop.csv")
# write.csv(tbl2, "nonHispanicPop.csv")


### Some code for the average CPI amount over past 20 years 
# cpi <- fread("BLSdataCPI.csv", header=TRUE, select=c(
#   "Year", 
#   "Annual"
# ))
# CPIchanges <- c((cpi$Annual[cpi$Year==2001] / cpi$Annual[cpi$Year==2000]), 
#                 (cpi$Annual[cpi$Year==2002] / cpi$Annual[cpi$Year==2001]), 
#                 (cpi$Annual[cpi$Year==2003] / cpi$Annual[cpi$Year==2002]), 
#                 (cpi$Annual[cpi$Year==2004] / cpi$Annual[cpi$Year==2003]), 
#                 (cpi$Annual[cpi$Year==2005] / cpi$Annual[cpi$Year==2004]), 
#                 (cpi$Annual[cpi$Year==2006] / cpi$Annual[cpi$Year==2005]), 
#                 (cpi$Annual[cpi$Year==2007] / cpi$Annual[cpi$Year==2006]), 
#                 (cpi$Annual[cpi$Year==2008] / cpi$Annual[cpi$Year==2007]), 
#                 (cpi$Annual[cpi$Year==2009] / cpi$Annual[cpi$Year==2008]), 
#                 (cpi$Annual[cpi$Year==2010] / cpi$Annual[cpi$Year==2009]), 
#                 (cpi$Annual[cpi$Year==2011] / cpi$Annual[cpi$Year==2010]), 
#                 (cpi$Annual[cpi$Year==2012] / cpi$Annual[cpi$Year==2011]), 
#                 (cpi$Annual[cpi$Year==2013] / cpi$Annual[cpi$Year==2012]), 
#                 (cpi$Annual[cpi$Year==2014] / cpi$Annual[cpi$Year==2013]), 
#                 (cpi$Annual[cpi$Year==2015] / cpi$Annual[cpi$Year==2014]), 
#                 (cpi$Annual[cpi$Year==2016] / cpi$Annual[cpi$Year==2015]), 
#                 (cpi$Annual[cpi$Year==2017] / cpi$Annual[cpi$Year==2016]), 
#                 (cpi$Annual[cpi$Year==2018] / cpi$Annual[cpi$Year==2017]), 
#                 (cpi$Annual[cpi$Year==2019] / cpi$Annual[cpi$Year==2018]), 
#                 (cpi$Annual[cpi$Year==2020] / cpi$Annual[cpi$Year==2019]))
# mean(CPIchanges)

### Analysis of IPEDS data to show that special-focus two-years and mixed baccalaurate/associate colleges are more diverse than other sectors. 
# library(data.table)
# library(dplyr)
# setwd("C:/Users/19732/Desktop/IPEDS/Files") 
# hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
#   "UNITID",    # Unique identification number of the institution
#   "INSTNM",    # Institution (entity) name
#   "CONTROL",   # Control of institution
#   "SECTOR",    # Sector of institution 
#   "C18BASIC",  # Carnegie Classification (Basic)
#   "OBEREG",    # Bureau of Economic Analysis (BEA) Regions
#   "TRIBAL",    # Tribal college
#   "STABBR"    # State abbreviation
# ))
# hd2020 <- hd2020 %>% filter(CONTROL==1)
# sfa1819 <- fread("sfa1819.csv", header=TRUE, select=c(
#   "UNITID",    # Unique identification number of the institution
#   "SCFA2",     # Total number of undergraduates - fall cohort
#   "UPGRNTN"    # Number of undergraduate students awarded Pell grants
# ))
# hd2020 <- left_join(x=hd2020, y=sfa1819, by="UNITID")
# effy2020 <- fread("effy2020.csv", header=TRUE, select=c(
#   "UNITID",   # Unique identification number of the institution
#   "EFFYALEV", # Level and degree/certificate-seeking status of student
#   "EFYTOTLT", # Grand total
#   "EFYAIANT", # American Indian or Alaska Native total
#   "EFYASIAT", # Asian total
#   "EFYBKAAT", # Black or African American total
#   "EFYHISPT", # Hispanic or Latino total
#   "EFYNHPIT", # Native Hawaiian or Other Pacific Islander total
#   "EFYWHITT", # White total
#   "EFY2MORT", # Two or more races total
#   "EFYUNKNT", # Race/ethnicity unknown total
#   "EFYNRALT"  # Nonresident alien total
# ))
# effy2020 <- effy2020 %>% filter(EFFYALEV == 1)
# effy2020$EFYTOTLT <- ifelse(is.na(effy2020$EFYTOTLT), 0, effy2020$EFYTOTLT)
# effy2020$EFYAIANT <- ifelse(is.na(effy2020$EFYAIANT), 0, effy2020$EFYAIANT)
# effy2020$EFYASIAT <- ifelse(is.na(effy2020$EFYASIAT), 0, effy2020$EFYASIAT)
# effy2020$EFYBKAAT <- ifelse(is.na(effy2020$EFYBKAAT), 0, effy2020$EFYBKAAT)
# effy2020$EFYHISPT <- ifelse(is.na(effy2020$EFYHISPT), 0, effy2020$EFYHISPT)
# effy2020$EFYNHPIT <- ifelse(is.na(effy2020$EFYNHPIT), 0, effy2020$EFYNHPIT)
# effy2020$EFYWHITT <- ifelse(is.na(effy2020$EFYWHITT), 0, effy2020$EFYWHITT)
# effy2020$EFY2MORT <- ifelse(is.na(effy2020$EFY2MORT), 0, effy2020$EFY2MORT)
# effy2020$EFYUNKNT <- ifelse(is.na(effy2020$EFYUNKNT), 0, effy2020$EFYUNKNT)
# effy2020$EFYNRALT <- ifelse(is.na(effy2020$EFYNRALT), 0, effy2020$EFYNRALT)
# fullData <- left_join(x=hd2020, y=effy2020, by="UNITID")
# carnegieList <- data.frame(c(
#   1,	# Associate's Colleges: High Transfer-High Traditional
#   2,	# Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional
#   3,	# Associate's Colleges: High Transfer-High Nontraditional
#   4,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-High Traditional
#   5,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-Mixed Traditional/Nontraditional
#   6,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-High Nontraditional
#   7,	# Associate's Colleges: High Vocational & Technical-High Traditional
#   8,	# Associate's Colleges: High Vocational & Technical-Mixed Traditional/Nontraditional
#   9,	# Associate's Colleges: High Vocational & Technical-High Nontraditional
#   14,	# Baccalaureate/Associate's Colleges: Associate's Dominant
#   33,	# Tribal Colleges
#   10,	# Special Focus Two-Year: Health Professions
#   11,	# Special Focus Two-Year: Technical Professions
#   12,	# Special Focus Two-Year: Arts & Design
#   13,	# Special Focus Two-Year: Other Fields
#   23	# Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's
# ), c(
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Other community colleges",
#   "Tribal Colleges",
#   "Special Focus Two-Years",
#   "Special Focus Two-Years",
#   "Special Focus Two-Years",
#   "Special Focus Two-Years",
#   "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's"
# ))
# names(carnegieList) <- c("C18BASIC", "C18BASIC2")
# fullData <- left_join(x=fullData, y=carnegieList, by="C18BASIC")
# tbl1.1 <- aggregate(data=fullData, EFYTOTLT ~ C18BASIC2, FUN=sum)
# tbl1.2 <- aggregate(data=fullData, EFYBKAAT ~ C18BASIC2, FUN=sum)
# tbl1.3 <- aggregate(data=fullData, EFYHISPT ~ C18BASIC2, FUN=sum)
# tbl1 <- full_join(x=tbl1.1, y=
#                     full_join(x=tbl1.2, y=tbl1.3, by="C18BASIC2"), 
#                   by="C18BASIC2")
# tbl1$blackShare <- tbl1$EFYBKAAT / tbl1$EFYTOTLT
# tbl1$hispcShare <- tbl1$EFYHISPT / tbl1$EFYTOTLT
# regions <- data.frame(c(0, # US Service schools
#                         1, # New England CT ME MA NH RI VT
#                         2, # Mid East DE DC MD NJ NY PA
#                         3, # Great Lakes IL IN MI OH WI
#                         4, # Plains IA KS MN MO NE ND SD
#                         5, # Southeast AL AR FL GA KY LA MS NC SC TN VA WV
#                         6, # Southwest AZ NM OK TX
#                         7, # Rocky Mountains CO ID MT UT WY
#                         8, # Far West AK CA HI NV OR WA
#                         9, # Outlying areas AS FM GU MH MP PR PW VI
#                         -3 # Not available
#                       ), c("US service schools", 
#                            "New England / Mid East", 
#                            "New England / Mid East", 
#                            "Great Lakes / Mid West", 
#                            "Great Lakes / Mid West", 
#                            "South", 
#                            "South", 
#                            "West", 
#                            "West", 
#                            "Outlying areas", 
#                            "Not available"
# ))
# names(regions) <- c("OBEREG", "OBEREG2")
# fullData <- left_join(x=fullData, y=regions, by="OBEREG")
# fullData$totalInstitutions <- rep(1, nrow(fullData))
# fullData$southernInstitutions <- ifelse(fullData$OBEREG2 == "South", 1, 0)
# tbl2.1 <- aggregate(data=fullData, totalInstitutions ~ C18BASIC2, FUN=sum)
# tbl2.2 <- aggregate(data=fullData, southernInstitutions ~ C18BASIC2, FUN=sum)
# tbl2 <- full_join(x=tbl2.1, y=tbl2.2, by="C18BASIC2")
# tbl2$southernShare <- tbl2$southernInstitutions / tbl2$totalInstitutions
# fullData2 <- fullData %>% filter(is.na(UPGRNTN)==FALSE & is.na(SCFA2)==FALSE)
# tbl3.1 <- aggregate(data=fullData2, UPGRNTN ~ C18BASIC2, FUN=sum)
# tbl3.2 <- aggregate(data=fullData2, SCFA2 ~ C18BASIC2, FUN=sum)
# tbl3 <- full_join(x=tbl3.1, y=tbl3.2, by="C18BASIC2")
# tbl3$pellShare <- tbl3$UPGRNTN / tbl3$SCFA2


####################
# 
# library(data.table)
# library(dplyr)
# library(ggplot2)
# setwd("C:/Users/19732/Desktop/IPEDS/Files") # This folder contains IPEDS data files and other necessary files. 
# 
# c2020_a <- fread("c2020_a.csv", header=TRUE, select=c(
#   "UNITID", 
#   "CIPCODE", 
#   "MAJORNUM", 
#   "AWLEVEL", 
#   "CTOTALT"
# ))
# c2020_a <- c2020_a %>% filter(CIPCODE==99)
# c2020_a <- c2020_a %>% filter(MAJORNUM==1)
# c2020_a <- c2020_a %>% filter(AWLEVEL %in% c(3, 5, 7, 17, 18, 19))
# tbl1All <- aggregate(data=c2020_a, CTOTALT ~ UNITID, FUN=sum)
# names(tbl1All) <- c("UNITID", "AllDegrees")
# tbl1Assoc <- c2020_a %>% filter(AWLEVEL==3)
# tbl1Assoc <- tbl1Assoc %>% select(UNITID, CTOTALT)
# names(tbl1Assoc) <- c("UNITID", "AssociateDegrees")
# hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
#   "UNITID", 
#   "INSTNM", 
#   "TRIBAL", 
#   "CONTROL"
# ))
# hd2020 <- hd2020 %>% filter(TRIBAL==1)
# fullData <- left_join(x=hd2020, y=tbl1All, by="UNITID")
# fullData <- left_join(x=fullData, y=tbl1Assoc, by="UNITID")
# fullData$associateShare <- fullData$AssociateDegrees / fullData$AllDegrees



# # Making a figure for report 
# 
# library(data.table)
# library(dplyr)
# library(ggplot2)
# setwd("C:/Users/19732/Documents/FedStatePartnershipApplication/Data")
# 
# runMoneyModel <- function(selectedYear, outputFile){
#   
#   stateData <- read.csv("stateModelA.csv", header=TRUE)
#   
#   CPIgrowthRate <- (2.063 / 100) + 1
#   
#   if(selectedYear=="Fiscal Year 2023-24"){
#     stateData$matchRate <- (100 / 100) 
#     inflationAdjustment <- CPIgrowthRate ^ 0
#   }
#   if(selectedYear=="Fiscal Year 2024-25"){
#     stateData$matchRate <- (95 / 100) 
#     inflationAdjustment <- CPIgrowthRate ^ 1
#   }
#   if(selectedYear=="Fiscal Year 2025-26"){
#     stateData$matchRate <- (90 / 100) 
#     inflationAdjustment <- CPIgrowthRate ^ 2
#   }
#   if(selectedYear=="Fiscal Year 2026-27"){
#     stateData$matchRate <- (85 / 100) 
#     inflationAdjustment <- CPIgrowthRate ^ 3
#   }
#   if(selectedYear=="Fiscal Year 2027-28"){
#     stateData$matchRate <- (80 / 100) 
#     inflationAdjustment <- CPIgrowthRate ^ 4
#   }
#   
#   stateData$federalAllocation <- stateData$partnershipCost * stateData$matchRate * inflationAdjustment 
#   stateData$federalAllocationPerFTE <- stateData$federalAllocation / stateData$adjResidentFTE
#   stateData$federalAllocationPerCapita <- stateData$federalAllocation / stateData$RESall
#   
#   stateData$stateContribution <- (stateData$partnershipCost * inflationAdjustment) * (1 - stateData$matchRate)
#   stateData$stateContribution <- ifelse(stateData$stateContribution < 0, 0, stateData$stateContribution)
#   
#   stateData$stateNewMoney <- stateData$stateContribution - stateData$stateFinAid
#   stateData$stateNewMoney <- ifelse(stateData$stateNewMoney < 0, 0, stateData$stateNewMoney)
#   
#   stateData$stateRemainder <- (stateData$tuitionFTEproduct * inflationAdjustment) - (stateData$federalAllocation + stateData$stateNewMoney)
#   stateData$extraContribution <- ifelse(stateData$stateRemainder >= 0, abs(stateData$stateRemainder), 0)
#   
#   stateData$overflow <- ifelse(stateData$stateRemainder < 0, abs(stateData$stateRemainder), 0)
#   
#   stateData$allNewMoney <- stateData$stateNewMoney + stateData$extraContribution
#   stateData$appropsIncrease <- (stateData$allNewMoney / (stateData$TwoYearEdApprops * inflationAdjustment))
#   
#   # forOutput <- stateData %>% select(STABBR2, stateRemainder)
#   write.csv(stateData, outputFile, row.names=FALSE)
#   rm(list = ls())
# }
# 
# runMoneyModel(selectedYear = "Fiscal Year 2023-24", outputFile = "fiscal2024numbers.csv")
# runMoneyModel(selectedYear = "Fiscal Year 2024-25", outputFile = "fiscal2025numbers.csv")
# runMoneyModel(selectedYear = "Fiscal Year 2025-26", outputFile = "fiscal2026numbers.csv")
# runMoneyModel(selectedYear = "Fiscal Year 2026-27", outputFile = "fiscal2027numbers.csv")
# runMoneyModel(selectedYear = "Fiscal Year 2027-28", outputFile = "fiscal2028numbers.csv")
# 
# fiscal2024 <- read.csv("fiscal2024numbers.csv", header=TRUE)
# fiscal2025 <- read.csv("fiscal2025numbers.csv", header=TRUE)
# fiscal2026 <- read.csv("fiscal2026numbers.csv", header=TRUE)
# fiscal2027 <- read.csv("fiscal2027numbers.csv", header=TRUE)
# fiscal2028 <- read.csv("fiscal2028numbers.csv", header=TRUE)
# 
# fiscal20282 <- fiscal2028 %>% select(STABBR2, appropsIncrease) %>% arrange(desc(appropsIncrease))
# fiscal20282$appropsIncrease[fiscal20282$STABBR2=="VT"] <- 0.9999
# names(fiscal20282) <- c("State", "Required Increase in Educational Appropriations to Two-Year Colleges")
# 
# ggplot(data=fiscal20282, mapping=aes(x = reorder(State, `Required Increase in Educational Appropriations to Two-Year Colleges`), y=`Required Increase in Educational Appropriations to Two-Year Colleges`))+geom_bar(stat="identity", width=0.4)+scale_y_continuous(limits=c(0, 1.00001), labels = scales::percent) + xlab("State") + ylab("Required Increase in Appropriations") + annotate("text", x = 47, y = 0.95, label = "VT: 234%")
# 
