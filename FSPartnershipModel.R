library(data.table)
library(dplyr)
library(ggplot2)
setwd("C:/Users/19732/Desktop/IPEDS/Files") # This folder contains IPEDS data files and other necessary files. 

runCostModel <- function(stateFileName, tribeFileName, specialFocus, mixedBacAssoc){
  
  #########################################################
  #### Step 1 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### First, we load in basic institutional           ####
  #### characteristics and remove all but the public   ####
  #### colleges and the tribal colleges (two tribal    ####
  #### colleges are private in IPEDS but I include     ####
  #### in the model for simplicity. We remove          ####
  #### system offices and institutions outside the 50  ####
  #### states or DC. This data set is the base for     #### 
  #### the analysis.                                   ####
  #########################################################
  
  hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "INSTNM",    # Institution (entity) name
    "CONTROL",   # Control of institution
    "SECTOR",    # Sector of institution 
    "C18BASIC",  # Carnegie Classification (Basic)
    "TRIBAL",    # Tribal college
    "STABBR"    # State abbreviation
  ))
  
  hd2020$STABBR <- ifelse(hd2020$TRIBAL==1, "TRIBAL", hd2020$STABBR) 
  hd2020 <- hd2020 %>% filter(CONTROL==1 | TRIBAL==1) 
  hd2020 <- hd2020 %>% filter(SECTOR>0)
  hd2020 <- hd2020 %>% filter((STABBR %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

  
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
  #### Next, we load in headcount enrollment by race.  ####
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
  effy2020 <- effy2020 %>% filter(EFFYALEV == 1)
  
  effy2020$EFYTOTLT <- ifelse(is.na(effy2020$EFYTOTLT), 0, effy2020$EFYTOTLT)
  effy2020$EFYAIANT <- ifelse(is.na(effy2020$EFYAIANT), 0, effy2020$EFYAIANT)
  effy2020$EFYASIAT <- ifelse(is.na(effy2020$EFYASIAT), 0, effy2020$EFYASIAT)
  effy2020$EFYBKAAT <- ifelse(is.na(effy2020$EFYBKAAT), 0, effy2020$EFYBKAAT)
  effy2020$EFYHISPT <- ifelse(is.na(effy2020$EFYHISPT), 0, effy2020$EFYHISPT)
  effy2020$EFYNHPIT <- ifelse(is.na(effy2020$EFYNHPIT), 0, effy2020$EFYNHPIT)
  effy2020$EFYWHITT <- ifelse(is.na(effy2020$EFYWHITT), 0, effy2020$EFYWHITT)
  effy2020$EFY2MORT <- ifelse(is.na(effy2020$EFY2MORT), 0, effy2020$EFY2MORT)
  effy2020$EFYUNKNT <- ifelse(is.na(effy2020$EFYUNKNT), 0, effy2020$EFYUNKNT)
  effy2020$EFYNRALT <- ifelse(is.na(effy2020$EFYNRALT), 0, effy2020$EFYNRALT)
  
  fullData <- left_join(x=hd2020, y=effy2020, by="UNITID")
  
  #########################################################
  #### We also need to pull in some data from finance  ####
  #### files for reference later on.                   ####
  #########################################################
  
  f1819_f1a <- fread("f1819_f1a.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "F1E03"      # Grants by state government
  ))
  f1819_f2 <- fread("f1819_f2.csv", header=TRUE, select=c(
    "UNITID",    # Unique identification number of the institution
    "F2C03"      # State grants 
  ))
  names(f1819_f1a) <- c("UNITID", "STATEGRANTS")
  names(f1819_f2) <- c("UNITID", "STATEGRANTS")
  f1819_f1a$STATEGRANTS[is.na(f1819_f1a$STATEGRANTS)] <- 0
  f1819_f2$STATEGRANTS[is.na(f1819_f2$STATEGRANTS)] <- 0
  
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
    "TUITION2",  # In-state average tuition for full-time undergraduates, 2020-21 
    "FEE2"       # In-state required fees for full-time undergraduates, 2020-21
  ))
  fullData <- left_join(x=fullData, y=ic2020_ay, by="UNITID")
  fullData$TUITION2 <- as.numeric(fullData$TUITION2)
  fullData$FEE2 <- as.numeric(fullData$FEE2)
  fullData$tuitionAndFees <- fullData$TUITION2 + fullData$FEE2
  
  #########################################################
  #### Step 3 of methodology:                          ####
  #########################################################
  
  #########################################################
  #### Next we define what counts as a community       ####
  #### college. Our method uses Carnegie               #### 
  #### Classifications and follows specifications      ####
  #### entered by the user in the app.                 ####
  #########################################################
  
  carnegieList <- c(
    1,	# Associate's Colleges: High Transfer-High Traditional
    2,	# Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional
    3,	# Associate's Colleges: High Transfer-High Nontraditional
    4,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-High Traditional
    5,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-Mixed Traditional/Nontraditional
    6,	# Associate's Colleges: Mixed Transfer/Vocational & Technical-High Nontraditional
    7,	# Associate's Colleges: High Vocational & Technical-High Traditional
    8,	# Associate's Colleges: High Vocational & Technical-Mixed Traditional/Nontraditional
    9,	# Associate's Colleges: High Vocational & Technical-High Nontraditional
    14,	# Baccalaureate/Associate's Colleges: Associate's Dominant
    33	# Tribal Colleges
  )
  if(specialFocus==TRUE){carnegieList <- c(
      carnegieList, 
      10,	# Special Focus Two-Year: Health Professions
      11,	# Special Focus Two-Year: Technical Professions
      12,	# Special Focus Two-Year: Arts & Design
      13	# Special Focus Two-Year: Other Fields
  )}
  if(mixedBacAssoc==TRUE){carnegieList <- c(
    carnegieList, 
    23	# Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's
  )}
  fullData <- fullData %>% filter(C18BASIC %in% carnegieList | UNITID==491844) 
  # The UNITID condition is included here because of Red Lake Nation College, which is missing from C18BASIC.
  
  #########################################################
  #### Step 4 of methodology:                          ####
  #########################################################
  
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
  fullData$tuitionAndFees[fullData$UNITID == 214795] <- collegeScorecard$TUITFTE[collegeScorecard$UNITID==214795] # Pennsylvania State University-Penn State Mont Alto (PA)
  
  fullData$tuitionAndFees <- as.numeric(fullData$tuitionAndFees)
  
  #########################################################
  #### Step 5 of methodology:                          ####
  #########################################################
  
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
    "SCFA13N"   # Number of students in fall cohort who are paying out-of-state tuition rates, 2018-19
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
  
  fullData$newResidentPct <- ifelse(is.na(fullData$residentPct)==FALSE, 
                             ifelse(is.na(fullData$inStatePct)==FALSE, 
                                    ((fullData$residentPct + fullData$inStatePct)*0.5), 
                                    fullData$residentPct), 
                              ifelse(is.na(fullData$inStatePct)==FALSE, 
                                     fullData$inStatePct, 1))
  
  fullData$residentFTE <- fullData$newResidentPct * fullData$totalFTE
  
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
  #### Next, we have to make certain adjustments.      ####
  #### First we account for inflation. Our numbers are ####
  #### based on 2019-20 tuition data, so we multiply   ####
  #### all costs by estimated change in CPI from 2019  ####
  #### to 2022. Next, we must account for less-than-   ####
  #### half-time enrollment. See full methodology for  ####
  #### the explanation, but we multiply FTEs by a fixed####
  #### coefficient, and we multiply headcount          ####
  #### enrollment by another (but different)           ####
  #### coefficient.                                    ####
  #########################################################
  
  fullData$adjTuitionAndFees <- fullData$tuitionAndFees * 1.04721063
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
  #### Now we find the average tuition amount, which   ####
  #### is a factor in the cost of the partnership.     ####
  #########################################################
  
  fullData$tuitionFTEproduct <- fullData$adjTuitionAndFees * fullData$adjResidentFTE
  avgTuition <- sum(fullData$tuitionFTEproduct) / sum(fullData$adjResidentFTE)
  
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
  names(stateFinAid) <- c("STABBR2", "STATEGRANTS")
  stateModel <- left_join(x=stateModel, y=stateFinAid, by="STABBR2")
  
  #########################################################
  #### We'll also need to know the "tuition target"    ####
  #### that reflects the cost of waiving tuition in    ####
  #### the state.                                      ####
  #########################################################
  
  stateModel <- left_join(x=stateModel, 
                          y=aggregate(data=fullStateData, tuitionFTEproduct ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  tribeModel <- left_join(x=tribeModel, 
                          y=aggregate(data=fullTribeData, tuitionFTEproduct ~ STABBR2, FUN=sum), 
                          by="STABBR2")
  
  # For Discussion Item 6
  # mean(fullStateData$adjTuitionAndFees) # Institutional definition
  # sum(fullStateData$tuitionFTEproduct) / sum(fullStateData$adjResidentFTE) # Student-level definition
  # mean(stateModel$tuitionFTEproduct / stateModel$adjResidentFTE) # State-level definition
  
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
  rm(list = ls())
}

runCostModel("stateModelA.csv", "tribeModel.csv", specialFocus=TRUE, mixedBacAssoc=TRUE)
runCostModel("stateModelB.csv", "tribeModel.csv", specialFocus=TRUE, mixedBacAssoc=FALSE)
runCostModel("stateModelC.csv", "tribeModel.csv", specialFocus=FALSE, mixedBacAssoc=TRUE)
runCostModel("stateModelD.csv", "tribeModel.csv", specialFocus=FALSE, mixedBacAssoc=FALSE)





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

library(data.table)
library(dplyr)
library(ggplot2)
setwd("C:/Users/19732/Desktop/IPEDS/Files") # This folder contains IPEDS data files and other necessary files. 

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
tbl1All <- aggregate(data=c2020_a, CTOTALT ~ UNITID, FUN=sum)
names(tbl1All) <- c("UNITID", "AllDegrees")
tbl1Assoc <- c2020_a %>% filter(AWLEVEL==3)
tbl1Assoc <- tbl1Assoc %>% select(UNITID, CTOTALT)
names(tbl1Assoc) <- c("UNITID", "AssociateDegrees")
hd2020 <- fread("hd2020.csv", header=TRUE, select=c(
  "UNITID", 
  "INSTNM", 
  "TRIBAL", 
  "CONTROL"
))
hd2020 <- hd2020 %>% filter(TRIBAL==1)
fullData <- left_join(x=hd2020, y=tbl1All, by="UNITID")
fullData <- left_join(x=fullData, y=tbl1Assoc, by="UNITID")
fullData$associateShare <- fullData$AssociateDegrees / fullData$AllDegrees
