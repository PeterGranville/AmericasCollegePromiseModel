library(data.table)
library(dplyr)
library(xlsx)
library(ggplot2)
setwd("C:/Users/19732/Desktop/IPEDS/Files")

runCostModel <- function(stateFileName, tribeFileName, assocThreshold, graduateFilter){
  
  #########################################################
  #### First, we load in basic institutional           ####
  #### characteristics and remove all but the public   ####
  #### colleges and the tribal colleges. We remove     ####
  #### system offices and institutions outside the 50  ####
  #### states as well. We also remove institutions     ####
  #### that offer graduate degrees, if that option is  ####
  #### selected in the arguments for runCostModel.     ####
  #### This data set is the base for the analysis.     ####
  #########################################################
  
  hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "INSTNM",    # Institution (entity) name
    "CONTROL",   # Control of institution
    "SECTOR",    # Sector of institution 
    "TRIBAL",    # Tribal college
    "STABBR",    # State abbreviation
    "GROFFER"    # Graduate offering 
  ))
  
  hd2020$STABBR <- ifelse(hd2020$TRIBAL==1, "TRIBAL", hd2020$STABBR) 
  hd2020 <- hd2020 %>% filter(CONTROL==1 | TRIBAL==1) 
  hd2020 <- hd2020 %>% filter(SECTOR>0)
  hd2020 <- hd2020 %>% filter((STABBR %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)
  if(graduateFilter==TRUE){
    hd2020 <- hd2020 %>% filter(GROFFER != 1)
  } # graduateFilter is an argument in runCostModel
  
  #########################################################
  #### Next, we set the proper tribe name for each     ####
  #### tribal college in STABBR2.                      ####
  #########################################################
  
  tribeNames <- fread("TCU Governance - Sheet1.csv", header=TRUE, select=c(
    "UNITID", 
    "TribeShortName"
  ))
  hd2020 <- left_join(x=hd2020, y=tribeNames, by="UNITID")
  hd2020$STABBR2 <- ifelse(is.na(hd2020$TribeShortName), hd2020$STABBR, hd2020$TribeShortName)
  
  #########################################################
  #### Next, we load in tuition and fees charges and   ####
  #### use a left-join to merge it to our base set.    ####
  #########################################################
  
  ic2020_ay <- fread("ic2020_ay.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "TUITION2",  # In-state average tuition for full-time undergraduates, 2020-21 
    "FEE2"       # In-state required fees for full-time undergraduates, 2020-21
  ))
  fullData <- left_join(x=hd2020, y=ic2020_ay, by="UNITID")
  fullData$TUITION2 <- as.numeric(fullData$TUITION2)
  fullData$FEE2 <- as.numeric(fullData$FEE2)
  fullData$tuitionAndFees <- fullData$TUITION2 + fullData$FEE2
  
  #########################################################
  #### Next we load in the numbers of awards by level  ####
  #### and by institution. We filter out second majors ####
  #### to treat a dual-major degree as just one degree.####
  #### We then determine what public institutions see  ####
  #### associate degrees comprise the majority of      ####
  #### degrees awarded in the most recent year. We     #### 
  #### define these as community colleges. We then     ####
  #### refine our base list to include only            ####
  #### community colleges and tribal colleges.         ####
  #########################################################
  
  c2020_a <- fread("c2020_a.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "CIPCODE",   # CIP Code -  2010 Classification
    "MAJORNUM",  # First or Second Major
    "AWLEVEL",   # Award Level code
    "CTOTALT"    # Grand total awarded between July 1, 2019 and June 30, 2020
  ))
  c2019_a <- fread("c2019_a.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "CIPCODE",   # CIP Code -  2010 Classification
    "MAJORNUM",  # First or Second Major
    "AWLEVEL",   # Award Level code
    "CTOTALT"    # Grand total awarded between July 1, 2018 and June 30, 2019
  ))
  c2018_a <- fread("c2018_a_rv.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "CIPCODE",   # CIP Code -  2010 Classification
    "MAJORNUM",  # First or Second Major
    "AWLEVEL",   # Award Level code
    "CTOTALT"    # Grand total awarded between July 1, 2017 and June 30, 2018
  ))
  c20xx_a <- rbind(c2018_a, c2019_a, c2020_a)
  c20xx_a <- c20xx_a %>% filter(MAJORNUM == 1) 
  c20xx_a <- left_join(x=c20xx_a, y=hd2020, by="UNITID")
  c20xx_a <- c20xx_a %>% filter(CONTROL==1)
  awardTotals1 <- aggregate(data=c20xx_a, CTOTALT ~ UNITID + AWLEVEL, FUN=sum)
  awardTotals1 <- awardTotals1 %>% filter(AWLEVEL %in% c(
    3,  # Associate's degrees 
    5,  # Bachelor's degrees 
    7,  # Master's degrees 
    17, # Doctor's degree (research/scholarship) 
    18, # Doctor's degree (professional practice) 
    19  # Doctor's degree (other) 
  )) # This removes all completion data except for degree completions (i.e. removes completion data on certificates). 
  awardTotals1$assoc <- ifelse(awardTotals1$AWLEVEL==3, awardTotals1$CTOTALT, 0)
  awardTotals2 <- aggregate(data=awardTotals1, CTOTALT ~ UNITID, FUN=sum)
  names(awardTotals2) <- c("UNITID", "allAwardsSum")
  assocShare <- left_join(x=awardTotals2, y=awardTotals1, by="UNITID")
  assocShare$assocPct <- assocShare$assoc / assocShare$allAwardsSum
  communityColleges <- aggregate(data=assocShare, assocPct ~ UNITID, FUN=sum)
  communityColleges <- communityColleges %>% filter(assocPct >= assocThreshold) #assocThreshold is an argument of runCostModel
  
  # Checking how my list compares to CCRC's 
  # forCCRCcomparison <- left_join(x=hd2020, y=communityColleges, by="UNITID")
  # CCRCsectors <- fread("CCRCsectors.csv", header=TRUE, select=c("UNITID", "SECTOR"))
  # sectorComp <- full_join(x=forCCRCcomparison, y=CCRCsectors, by="UNITID")
  # write.csv(sectorComp, "output0726202101.csv")
  
  fullData <- left_join(x=fullData, y=communityColleges, by="UNITID")
  fullData <- fullData %>% filter(is.na(assocPct)==FALSE | TRIBAL==1)
  
  #########################################################
  #### Next, we must address missing tuition revenue   ####
  #### for some institutions. We use College Scorecard ####
  #### tuition revenue per FTE as a proxy. This is     ####
  #### only necessary for 15 institutions in our set.  ####
  #########################################################
  
  collegeScorecard <- fread("Most-Recent-Cohorts-All-Data-Elements.csv", header=TRUE, select=c("UNITID", "TUITFTE"))
  fullData$tuitionAndFees[fullData$UNITID == 155618] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==155618]  # Northwest Kansas Technical College (KS)
  fullData$tuitionAndFees[fullData$UNITID == 369668] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==369668]  # Central Pennsylvania Institute of Science and Technology (PA)
  fullData$tuitionAndFees[fullData$UNITID == 383084] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==383084]  # Hacienda La Puente Adult Education (CA)
  fullData$tuitionAndFees[fullData$UNITID == 413802] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==413802]  # East San Gabriel Valley Regional Occupational Program (CA)
  fullData$tuitionAndFees[fullData$UNITID == 418533] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==418533]  # Lancaster County Career and Technology Center (PA)
  fullData$tuitionAndFees[fullData$UNITID == 430795] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==430795]  # Carver Career Center (WV)
  fullData$tuitionAndFees[fullData$UNITID == 491844] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==491844]  # Red Lake Nation College
  fullData$tuitionAndFees[fullData$UNITID == 214740] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==214740] # Pennsylvania State University-Penn State DuBois
  fullData$tuitionAndFees[fullData$UNITID == 223524] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==223524] # Brookhaven College
  fullData$tuitionAndFees[fullData$UNITID == 223773] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==223773] # Cedar Valley College
  fullData$tuitionAndFees[fullData$UNITID == 224572] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==224572] # Eastfield College
  fullData$tuitionAndFees[fullData$UNITID == 226930] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==226930] # Mountain View College
  fullData$tuitionAndFees[fullData$UNITID == 227191] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==227191] # North Lake College
  fullData$tuitionAndFees[fullData$UNITID == 227766] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==227766] # Richland College
  fullData$tuitionAndFees[fullData$UNITID == 439145] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==439145] # Pierce College-Puyallup
  
  fullData$tuitionAndFees <- as.numeric(fullData$tuitionAndFees)
  
  #########################################################
  #### We now have tuition and fee revenue for every   ####
  #### community college and tribal college. We next   ####
  #### load in enrollment data. I verified by hand that####
  #### every institution in IPEDS that is in our set   ####
  #### has either undergraduate or graduate FTE data.  ####
  #### After removing NAs, the sum of undergrad FTEs   ####
  #### and graduate FTEs is the correct total FTEs.    ####
  #########################################################
  
  efia2020 <- fread("efia2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "EFTEUG",    # Estimated full-time equivalent (FTE) undergraduate enrollment, 2018-19
    "EFTEGD"     # Estimated full-time equivalent (FTE) graduate enrollment, 2018-19
  ))
  fullData <- left_join(x=fullData, y=efia2020, by="UNITID")
  # fullData[is.na(fullData$EFTEUG) & is.na(fullData$EFTEGD)]
  # This shows that all institutions in fullData have either UG or GD FTE data. 
  fullData$EFTEUG[is.na(fullData$EFTEUG)] <- 0
  fullData$EFTEGD[is.na(fullData$EFTEGD)] <- 0
  fullData$EFTEUG <- as.numeric(fullData$EFTEUG)
  fullData$EFTEGD <- as.numeric(fullData$EFTEGD)
  fullData$totalFTE <- fullData$EFTEUG + fullData$EFTEGD
  # fullData[is.na(fullData$totalFTE)]
  # This shows that our totalFTE variable does not have any missing points. 
  
  #########################################################
  ####            Attempt 1 at in-state PCT            ####
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
  ####            Attempt 2 at in-state PCT            ####
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
    "SCFA13N"  # Number of students in fall cohort who are paying out-of-state tuition rates, 2018-19
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
  
  # For item 2 in the methodology: 
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
  
  fullData$newResidentPct <- ifelse(is.na(fullData$residentPct)==FALSE, 
                             ifelse(is.na(fullData$inStatePct)==FALSE, 
                                    ((fullData$residentPct + fullData$inStatePct)*0.5), 
                                    fullData$residentPct), 
                              ifelse(is.na(fullData$inStatePct)==FALSE, 
                                     fullData$inStatePct, 1))
  fullData$residentFTE <- fullData$newResidentPct * fullData$totalFTE
  
  #########################################################
  #### Next, we have to make certain adjustments.      ####
  #### First we account for inflation. Our numbers are ####
  #### based on 2019-20 tuition data, so we multiply   ####
  #### all costs by estimated change in CPI from 2019  ####
  #### to 2022. Next, we must account for less-than-   ####
  #### half-time enrollment. See full methodology for  ####
  #### the explanation, but we multiply FTEs by a fixed####
  #### coefficient.                                    ####
  #########################################################
  
  fullData$adjTuitionAndFees <- fullData$tuitionAndFees * 1.04721063
  fullData$adjResidentFTE <- fullData$residentFTE * 0.9459854 
  
  #########################################################
  #### Now we find the average tuition amount, which   ####
  #### is a factor in the cost of the partnership.     ####
  #########################################################
  
  fullData$tuitionFTEproduct <- fullData$adjTuitionAndFees * fullData$adjResidentFTE
  avgTuition <- sum(fullData$tuitionFTEproduct) / sum(fullData$adjResidentFTE)
  
  #########################################################
  #### Before we move from institution-level data      ####
  #### to a state-level model, we need to add in info  ####
  #### about the location of Title III MSIs.           ####
  #### Note that the TCU list matches the IPEDS list   ####
  #### of TCUs. I load the TCU list anyway, though it  ####
  #### is technically redundant.                       ####
  #########################################################
  
  AANAPISI <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 2)[, c(3, 1)]
  ANNH <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 3)[, c(3, 1)]
  HBCU <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 4)[, c(3, 1)]
  HSI <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 5)[, c(3, 1)]
  NASNTI <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 6)[, c(3, 1)]
  PBI <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 7)[, c(3, 1)]
  TCU <- read.xlsx("2020 CMSI Eligibility Matrix.xlsx", sheetIndex = 8)[, c(3, 1)]
  names(AANAPISI) <- c("UNITID", "AANAPISI")
  names(ANNH) <- c("UNITID", "ANNH")
  names(HBCU) <- c("UNITID", "HBCU")
  names(HSI) <- c("UNITID", "HSI")
  names(NASNTI) <- c("UNITID", "NASNTI")
  names(PBI) <- c("UNITID", "PBI")
  names(TCU) <- c("UNITID", "TCU")
  
  fullData <- left_join(x=fullData, y=AANAPISI, by="UNITID")
  fullData <- left_join(x=fullData, y=ANNH, by="UNITID")
  fullData <- left_join(x=fullData, y=HBCU, by="UNITID")
  fullData <- left_join(x=fullData, y=HSI, by="UNITID")
  fullData <- left_join(x=fullData, y=NASNTI, by="UNITID")
  fullData <- left_join(x=fullData, y=PBI, by="UNITID")
  fullData <- left_join(x=fullData, y=TCU, by="UNITID")
  
  AANAPISIbyState <- aggregate(data=fullData[is.na(fullData$AANAPISI)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  ANNHbyState <- aggregate(data=fullData[is.na(fullData$ANNH)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  HBCUbyState <- aggregate(data=fullData[is.na(fullData$HBCU)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  HSIbyState <- aggregate(data=fullData[is.na(fullData$HSI)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  NASNTIbyState <- aggregate(data=fullData[is.na(fullData$NASNTI)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  PBIbyState <- aggregate(data=fullData[is.na(fullData$PBI)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  TCUbyState <- aggregate(data=fullData[is.na(fullData$TCU)==FALSE], adjResidentFTE ~ STABBR2, FUN=sum)
  
  names(AANAPISIbyState) <- c("STABBR2", "AANAPISIfte")
  names(ANNHbyState) <- c("STABBR2", "ANNHfte")
  names(HBCUbyState) <- c("STABBR2", "HBCUfte")
  names(HSIbyState) <- c("STABBR2", "HSIfte")
  names(NASNTIbyState) <- c("STABBR2", "NASNTIfte")
  names(PBIbyState) <- c("STABBR2", "PBIfte")
  names(TCUbyState) <- c("STABBR2", "TCUfte")
  
  MSIbyState <- full_join(x=AANAPISIbyState, y=
                  full_join(x=ANNHbyState, y=
                    full_join(x=HBCUbyState, y=
                        full_join(x=HSIbyState, y=
                            full_join(x=NASNTIbyState, y=
                                full_join(x=PBIbyState, y=TCUbyState, by="STABBR2"), 
                            by="STABBR2"), 
                        by="STABBR2"), 
                    by="STABBR2"), 
                  by="STABBR2"), 
                by="STABBR2")
  
  
  #########################################################
  #### Now we aggregate data by state/tribe.           ####
  #########################################################
  
  fullStateData <- fullData %>% filter(TRIBAL==2)
  fullTribeData <- fullData %>% filter(TRIBAL==1)
  
  stateModel <- aggregate(data=fullStateData, adjResidentFTE ~ STABBR2, FUN=sum) 
  tribeModel <- aggregate(data=fullTribeData, adjResidentFTE ~ STABBR2, FUN=sum)
  
  stateModel$partnershipCost <- stateModel$adjResidentFTE * avgTuition
  tribeModel$partnershipCost <- tribeModel$adjResidentFTE * avgTuition
  
  stateModel <- left_join(x=stateModel, y=MSIbyState, by="STABBR2")
  tribeModel <- left_join(x=tribeModel, y=MSIbyState, by="STABBR2")
  
  # For item 6
  # mean(fullStateData$adjTuitionAndFees) # Institutional definition
  # sum(fullStateData$tuitionFTEproduct) / sum(fullStateData$adjResidentFTE) # Student-level definition
  # mean(stateModel$tuitionFTEproduct / stateModel$adjResidentFTE) # State-level definition
  
  #########################################################
  #### I now add in data that will be necessary in the ####
  #### R Shiny code when calculating the federal match ####
  #### for tribes. See Item 6 for details.             ####
  #########################################################
  
  tribeModel <- left_join(x=tribeModel, 
                          y=aggregate(data=fullTribeData, tuitionFTEproduct ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  
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
    "HSwhite",	     # White enrollment, grades 9-12
    "HSblack",       # Black enrollment, grades 9-12
    "HShispanic",    # Hispanic enrollment, grades 9-12
    "HSaapi",        # AAPI enrollment, grades 9-12
    "HSnative",	     # Native American / Alaska Native enrollment, grades 9-12
    "HStwo",         # Two or more races / Other race enrollment, grades 9-12
    "RESwhite",	     # White residents aged 18-35 without a college degree
    "RESblack",      # Black residents aged 18-35 without a college degree
    "REShispanic",	 # Hispanic residents aged 18-35 without a college degree 
    "RESaapi",       # AAPI residents aged 18-35 without a college degree
    "RESnative",	   # Native American / Alaska Native residents aged 18-35 without a college degree
    "REStwo"         # Two or more races / Other race residents aged 18-35 without a college degree 
  ))
  stateModel <- left_join(x=stateModel, y=stateVars, by="STABBR2")
  
  #########################################################
  #### We have what we need for the interactive model, ####
  #### so we now export the state and tribe model sets ####
  #### as csv files.                                   ####
  #########################################################
  
  write.csv(stateModel, stateFileName, row.names=FALSE)
  write.csv(tribeModel, tribeFileName, row.names=FALSE)
  rm(list = ls())
}

runCostModel("stateModel50A.csv", "tribeModel50A.csv", assocThreshold=0.5, graduateFilter=FALSE)
runCostModel("stateModel50B.csv", "tribeModel50B.csv", assocThreshold=0.5, graduateFilter=TRUE)

runCostModel("stateModel60A.csv", "tribeModel60A.csv", assocThreshold=0.6, graduateFilter=FALSE)
runCostModel("stateModel60B.csv", "tribeModel60B.csv", assocThreshold=0.6, graduateFilter=TRUE)

runCostModel("stateModel70A.csv", "tribeModel70A.csv", assocThreshold=0.7, graduateFilter=FALSE)
runCostModel("stateModel70B.csv", "tribeModel70B.csv", assocThreshold=0.7, graduateFilter=TRUE)

runCostModel("stateModel80A.csv", "tribeModel80A.csv", assocThreshold=0.8, graduateFilter=FALSE)
runCostModel("stateModel80B.csv", "tribeModel80B.csv", assocThreshold=0.8, graduateFilter=TRUE)

runCostModel("stateModel90A.csv", "tribeModel90A.csv", assocThreshold=0.9, graduateFilter=FALSE)
runCostModel("stateModel90B.csv", "tribeModel90B.csv", assocThreshold=0.9, graduateFilter=TRUE)

runCostModel("stateModel95A.csv", "tribeModel95A.csv", assocThreshold=0.95, graduateFilter=FALSE)
runCostModel("stateModel95B.csv", "tribeModel95B.csv", assocThreshold=0.95, graduateFilter=TRUE)




# ACS Data Pull 
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













