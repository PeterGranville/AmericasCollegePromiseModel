library(shiny)
library(dplyr)
library(scales)
library(plotly)
library(DT)

ui <- fluidPage(
    
    titlePanel("America's College Promise Federal-State Partnership"),
    h5("In the lefthand panel, select key design inputs for the federal-state partnership for tuition-free community college. You will then see effects on new federal funding, the funding distribution to states and tribes, and breakdowns by subgroups. This model is based on the America's College Promise Act of 2021 (Baldwin/Levin). To share questions or feedback, please email granville@tcf.org."),

    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Basic inputs", fluid=TRUE,
                    h5(em("Policy Design Choices: Defining Eligible Institutions")),
                    h6("Our partnership model includes public associate's colleges and associate's-dominant public baccalaureate/associate's colleges. This captures most institutions that function as community colleges. For other groups of colleges, however, the 'community college' designation is less clear-cut. In designing ACP, Congress could choose bill language that is either inclusive or exclusive of the following groups."),
                    selectInput(
                            inputId = "specialFocus", 
                            label = "Should public special-focus two-year colleges be counted as community colleges?", 
                            choices=c(
                                "Yes", 
                                "No")
                    ),
                    h6("Selecting 'No' will remove 10 campuses from the partnership model. Examples include Northwest Louisiana Technical Community College and Nebraska College of Technical Agriculture."),
                    selectInput(
                        inputId = "mixedBacAssoc", 
                        label = "Should public mixed baccalaureate/associate colleges be counted as community colleges?", 
                        choices=c(
                            "Yes", 
                            "No")
                    ), 
                    h6("Selecting 'No' will remove 41 campuses from the partnership model. Examples include Miami Dade College, CUNY Medgar Evers College, and Vermont Technical College."),
                    h5(em("Policy Design Choices: Federal Share of Costs")),
                    sliderInput(
                        inputId="averageMatch", 
                        label="What should be the federal share of partnership costs?",
                        min=50, 
                        max=100, 
                        value=75,
                        step=1, 
                        post="%"
                    ),
                    h6("This determines the federal government's share of total costs. If you select no adjustment factors below, then every state receives funding equal to this percentage of costs to eliminate tuition and fees in that state. If you select adjustment factors below, the federal government will cover a greater or lesser share of each state's costs according to those factors."),
                    checkboxGroupInput(
                        inputId="adjustmentFactors", 
                        label="Which (if any) of these factors should determine each state's match rate?", 
                        choices=c("The state's wealth"="stateWealth", 
                                  "The state's child poverty rate"="childPoverty", 
                                  "The state's personal income per capita"="personalIncome", 
                                  "The share of the state's K-12 students who are eligible for FRPL"="FRPLshare", 
                                  "The share of the state's public undergraduates who receive Pell"="pellShare")
                    ),
                    h6("Choose any combination of factors. If you want the federal match rate to be equal across all states, do not check any boxes."),
                    sliderInput(
                        inputId="powerCoefficient", 
                        label="Select how heavily you want the adjustment factors to influence the match rate.", 
                        min=0,
                        max=2, 
                        value=1,
                        step=0.25
                    ),
                    h6("This value goes into a match rate formula, similar to the formula used for Medicaid funding. A value of 0 gives your selected factors no weight. The higher the value, the greater variation there will be among states' match rates.")
                ), 
                
                tabPanel("Advanced inputs", fluid=TRUE,
                    h5(em("Additional Cost Controls")),
                    selectInput(
                        inputId = "bounds", 
                        label = "Would you like to apply bounds to the federal share of partnership costs per state?", 
                        choices=c(
                            "Yes", 
                            "No")
                    ),
                    sliderInput(
                        inputId="minmaxFS", 
                        label="If you chose 'Yes' above, choose your desired mininum and maximum values for the federal share of partnership costs per state.", 
                        min=0,
                        max=150, 
                        value=c(0, 100),
                        step=1, 
                        post="%"
                     ),   
                    h6("Think of these as bounds on the match rate. The slider can go above 100% because some states, including many poorer states, have tuition and fee prices above the national average."),
                    h5(em("Additional Assumptions")),
                    selectInput(
                        inputId = "medicaid", 
                        label = "Will the 12 states that have not adopted Medicaid expansion under the ACA choose to participate in ACP?", 
                        choices=c(
                            "Yes", 
                            "No")
                    ),
                    h6("These states are Alabama, Florida, Georgia, Kansas, Mississippi, North Carolina, South Carolina, South Dakota, Tennessee, Texas, Wisconsin, and Wyoming."),
                    sliderInput(
                        inputId="averageCPIgrowth", 
                        label="Specify the average CPI inflation rate you expect over the next ten years.", 
                        min=1.000,
                        max=3.000, 
                        value=2.063,
                        step=0.001, 
                        post="%"
                    ),
                    h6("The default, 2.063%, is the average annual growth from 2000 to 2020."),
                    sliderInput(
                        inputId="covidEffect", 
                        label="In 2020-21, community college enrollment was down about 9.8% from 2019-20 due to the COVID-19 pandemic. Should the model expect baseline enrollment to be down when ACP begins in 2022-23? If so, specify the size of the drop compared to 2019-20.", 
                        min=0.0,
                        max=10.0, 
                        value=0.0,
                        step=0.1, 
                        post="%"
                    ),
                    sliderInput(
                        inputId="inducementEffect", 
                        label="Do you think establishing ACP will lead to an increase in community college enrollment? If so, specify how large that effect will be.", 
                        min=0.0,
                        max=10.0, 
                        value=0.0,
                        step=0.1, 
                        post="%"
                    ),
                    h6("It's impossible to know what this effect will be, so take your best guess."),
                    h5(em("Additional Partnership Features")),
                    numericInput(inputId="blockGrants", 
                                 label="Do you want ACP to include block grants to states with few or no community colleges or to U.S. territories? If so, enter here the total dollar amount of any additional federal block grants in the first year of ACP.", 
                                 value=0, 
                                 min=0, 
                                 step=10000000), 
                    h6("Of the charts and tables to the right, only Table 2 will be affected by whatever number you enter here."), 
                    selectInput(
                        inputId = "californiaPromiseGrant", 
                        label = "Does the California College Promise Grant (its community college fee waiver program) count as state financial aid?", 
                        choices=c(
                            "Yes", 
                            "No")
                    ),
                    h6("This is an example of a unique state policy meshing awkwardly with federal policy. If you select yes, then California's cost to participate in ACP will be reduced. This affects only Tables 8 and 9.")
                ), 
                tabPanel("State fin. aid", fluid=TRUE,
                         h6("A state may include in its share of ACP costs the amount of need-based financial aid it delivers to ACP-eligible community college students to pay for non-tuition expenses (see Sec. 499B(b)(2) of the America's College Promise Act of 2021). Assuming that each state converts its existing financial aid for community college tuition into stipends for community college students, then the amount of existing state financial aid affects how much new money states would need to appropriate to join ACP."), 
                         h6("This model uses the latest IPEDS data on state financial aid received by community college students, which reflects FY 2018-19. Because a state's appropriations for financial aid can meaningfully change within two years, this app allows users the option to manually adjust the model's total state financial aid delivered to community college students in a state. The default values reflect the sums derived from IPEDS data for 2018-19 for all community colleges, including public special-focus two-years and public mixed baccalaureate/associate colleges."), 
                         h6("Adjusting the values below will affect the results in Tables 8 and 9."), 
                         numericInput(inputId="alabama", 
                                      label="Alabama:", 
                                      value=13976588, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="arizona", 
                                      label="Arizona:", 
                                      value=1111165, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="arkansas", 
                                      label="Arkansas:", 
                                      value=8384563, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="california", 
                                      label="California:", 
                                      value=691422853, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="colorado", 
                                      label="Colorado:", 
                                      value=47605959, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="connecticut", 
                                      label="Connecticut:", 
                                      value=9744558, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="delaware", 
                                      label="Delaware:", 
                                      value=1095581, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="florida", 
                                      label="Florida:", 
                                      value=138479499, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="georgia", 
                                      label="Georgia:", 
                                      value=31836303, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="hawaii", 
                                      label="Hawaii:", 
                                      value=358479, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="idaho", 
                                      label="Idaho:", 
                                      value=1988900, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="illinois", 
                                      label="Illinois:", 
                                      value=55585061, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="indiana", 
                                      label="Indiana:", 
                                      value=36529327, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="iowa", 
                                      label="Iowa:", 
                                      value=10565504, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="kansas", 
                                      label="Kansas:", 
                                      value=2676791, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="kentucky", 
                                      label="Kentucky:", 
                                      value=32133951, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="louisiana", 
                                      label="Louisiana:", 
                                      value=3222951, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="maine", 
                                      label="Maine:", 
                                      value=4206824, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="maryland", 
                                      label="Maryland:", 
                                      value=9159058, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="massachusetts", 
                                      label="Massachusetts:", 
                                      value=28358425, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="michigan", 
                                      label="Michigan:", 
                                      value=14387195, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="minnesota", 
                                      label="Minnesota:", 
                                      value=43638000, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="mississippi", 
                                      label="Mississippi:", 
                                      value=7484446, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="missouri", 
                                      label="Missouri:", 
                                      value=16068150, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="montana", 
                                      label="Montana:", 
                                      value=187722, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="nebraska", 
                                      label="Nebraska:", 
                                      value=4275673, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="nevada", 
                                      label="Nevada:", 
                                      value=10243000, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="newHampshire", 
                                      label="New Hampshire:", 
                                      value=2896000, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="newJersey", 
                                      label="New Jersey:", 
                                      value=52144299, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="newMexico", 
                                      label="New Mexico:", 
                                      value=9397759, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="newYork", 
                                      label="New York:", 
                                      value=276455757, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="northCarolina", 
                                      label="North Carolina:", 
                                      value=36124243, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="northDakota", 
                                      label="North Dakota:", 
                                      value=2739834, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="ohio", 
                                      label="Ohio:", 
                                      value=6337505, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="oklahoma", 
                                      label="Oklahoma:", 
                                      value=18614116, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="oregon", 
                                      label="Oregon:", 
                                      value=46463250, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="pennsylvania", 
                                      label="Pennsylvania:", 
                                      value=32123170, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="rhodeIsland", 
                                      label="Rhode Island:", 
                                      value=9037237, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="southCarolina", 
                                      label="South Carolina:", 
                                      value=114604551, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="southDakota", 
                                      label="South Dakota:", 
                                      value=688810, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="tennessee", 
                                      label="Tennessee:", 
                                      value=124475038, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="texas", 
                                      label="Texas:", 
                                      value=64070232, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="utah", 
                                      label="Utah:", 
                                      value=1017337, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="vermont", 
                                      label="Vermont:", 
                                      value=1655092, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="virginia", 
                                      label="virginia:", 
                                      value=51394837, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="washington", 
                                      label="Washington:", 
                                      value=120736268, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="westVirginia", 
                                      label="West Virginia:", 
                                      value=7296498, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="wisconsin", 
                                      label="Wisconsin:", 
                                      value=24199263, 
                                      min=0, 
                                      step=1),
                         numericInput(inputId="wyoming", 
                                      label="Wyoming:", 
                                      value=6455836, 
                                      min=0, 
                                      step=1)
                )
            ),
            submitButton(text = "Enter my selections", icon = NULL, width = NULL)
        ),

        mainPanel(
           plotlyOutput("map1"),
           h4("Table 1: America's College Promise basics"),
           tableOutput("table0"),
           h6("Enrollment numbers refer to the first year of the partnership, 2022-23."),
           h4("Table 2: New federal funding for community colleges"),
           tableOutput("table2"),
           plotlyOutput("chart1"),
           h4("Table 3: Federal allocations to states (FY 2022-23)"),
           dataTableOutput("table1S"), 
           h4("Table 4: Federal allocations to tribes (FY 2022-23)"),
           dataTableOutput("table1T"),
           h4("Table 5: New federal funding for community colleges by students' race/ethnicity (FY 2022-23)"),
           tableOutput("table3A"), 
           h6("This is a headcount measure, so the average per student here is lower than average per FTE student. Each row besides 'Hispanic' and 'Two or more races / Other' refers exclusively to non-Hispanic students."),
           h4("Table 6: New federal funding for community colleges by residents' race/ethnicity (FY 2022-23)"),
           tableOutput("table3B"), 
           h6("Due to data limitations, the table above only reflects money to states and not tribes. Each row besides 'Hispanic' and 'Two or more races / Other' refers exclusively to non-Hispanic residents."),
           h4("Table 7: New federal funding by Title III Minority-Serving Institution (MSI) category (FY 2022-23)"), 
           tableOutput("table4"), 
           h4("Table 8: States' cost to participate and waive community college tuition and fees (FY 2022-23)"),
           dataTableOutput("table9"),
           h6("Overflow money refers to the ACP funding that remains after tuition and fees have been waived through the federal contribution and state contribution. In many states, this is $0. Some states, however, have a balance of ACP funding that they can apply towards certain purposes specified in the America's College Promise Act bill text. These purposes include enhancing the quality and equity of public higher education, investing in and diversifying the academic workforce, expanding high-quality skills training programs at community colleges, and more. See Section 499E(b) of the bill for the full list of allowable uses."),
           h4("Table 9: Summary of funding streams (FY 2022-23)"),
           tableOutput("table010"), 
           h6("The America's College Promise Act incentivizes states to convert existing financial aid for community college tuition into non-tuition aid for community college students. It does this by allowing a state's non-tuition aid for community college students to offset the state's participation costs. This is why some states have $0 participation costs in Table 8. This aid is summed in Table 9.")
        )
    )
)

server <- function(input, output) {

    observe({
        
        ###############################################################
        #### Step 1 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### First we determine which state/tribal files to use    ####
        #### based on the inputs submitted by the user.            ####
        ###############################################################
        
        if(input$specialFocus=="Yes" & input$mixedBacAssoc=="Yes"){
            stateFile <- "Data/stateModelA.csv"
        }
        if(input$specialFocus=="Yes" & input$mixedBacAssoc=="No"){
            stateFile <- "Data/stateModelB.csv"
        }
        if(input$specialFocus=="No" & input$mixedBacAssoc=="Yes"){
            stateFile <- "Data/stateModelC.csv"
        }
        if(input$specialFocus=="No" & input$mixedBacAssoc=="No"){
            stateFile <- "Data/stateModelD.csv"
        }
        tribeFile <- "Data/tribeModel.csv"
        stateData <- read.csv(stateFile, header=TRUE)
        tribeData <- read.csv(tribeFile, header=TRUE)
        
        ###############################################################
        #### We start by taking in the state financial aid totals  ####
        #### that may (or may not have) been adjusted by the user. ####
        ###############################################################
        
        stateFinAid <- data.frame(c(
            "AL", # input$alabama
            "AR", # input$arkansas
            "AZ", # input$arizona
            "CA", # input$california
            "CO", # input$colorado
            "CT", # input$connecticut
            "DE", # input$delaware
            "FL", # input$florida
            "GA", # input$georgia
            "HI", # input$hawaii
            "IA", # input$iowa
            "ID", # input$idaho
            "IL", # input$illinois
            "IN", # input$indiana
            "KS", # input$kansas
            "KY", # input$kentucky
            "LA", # input$louisiana
            "MA", # input$massachusetts
            "MD", # input$maryland
            "ME", # input$maine
            "MI", # input$michigan
            "MN", # input$minnesota
            "MO", # input$missouri
            "MS", # input$mississippi
            "MT", # input$montana
            "NC", # input$northCarolina
            "ND", # input$northDakota
            "NE", # input$nebraska
            "NH", # input$newHampshire
            "NJ", # input$newJersey
            "NM", # input$newMexico
            "NV", # input$nevada
            "NY", # input$newYork
            "OH", # input$ohio
            "OK", # input$oklahoma
            "OR", # input$oregon
            "PA", # input$pennsylvania
            "RI", # input$rhodeIsland
            "SC", # input$southCarolina
            "SD", # input$southDakota
            "TN", # input$tennessee
            "TX", # input$texas
            "UT", # input$utah
            "VA", # input$virginia
            "VT", # input$vermont
            "WA", # input$washington
            "WI", # input$wisconsin
            "WV", # input$westVirginia
            "WY"  # input$wyoming
        ), c(
            input$alabama,
            input$arkansas,
            input$arizona,
            input$california,
            input$colorado,
            input$connecticut,
            input$delaware,
            input$florida,
            input$georgia,
            input$hawaii,
            input$iowa,
            input$idaho,
            input$illinois,
            input$indiana,
            input$kansas,
            input$kentucky,
            input$louisiana,
            input$massachusetts,
            input$maryland,
            input$maine,
            input$michigan,
            input$minnesota,
            input$missouri,
            input$mississippi,
            input$montana,
            input$northCarolina,
            input$northDakota,
            input$nebraska,
            input$newHampshire,
            input$newJersey,
            input$newMexico,
            input$nevada,
            input$newYork,
            input$ohio,
            input$oklahoma,
            input$oregon,
            input$pennsylvania,
            input$rhodeIsland,
            input$southCarolina,
            input$southDakota,
            input$tennessee,
            input$texas,
            input$utah,
            input$virginia,
            input$vermont,
            input$washington,
            input$wisconsin,
            input$westVirginia,
            input$wyoming
        ))
        names(stateFinAid) <- c("STABBR2", "STATEGRANTS2")
        stateData <- left_join(x=stateData, y=stateFinAid, by="STABBR2")
        
        ###############################################################
        #### Before working with the data further, we account for  ####
        #### inducement effects and effects of the COVID-19        ####
        #### pandemic on community college enrollment. If the user ####
        #### has not moved from the default, then no effects are   ####
        #### applied (i.e. enrollment data are multiplied by 1).   ####  
        ###############################################################
        
        changeFromEnrollBase <- (1 - (input$covidEffect / 100)) * (1 + (input$inducementEffect / 100))
        
        stateData$adjResidentFTE <- stateData$adjResidentFTE * changeFromEnrollBase  
        stateData$partnershipCost <- stateData$partnershipCost * changeFromEnrollBase  
        stateData$AANAPIIfte <- stateData$AANAPIIfte * changeFromEnrollBase  
        stateData$ANNHfte <- stateData$ANNHfte * changeFromEnrollBase  
        stateData$HBCUfte <- stateData$HBCUfte * changeFromEnrollBase  
        stateData$HSIfte <- stateData$HSIfte * changeFromEnrollBase  
        stateData$NANTIfte <- stateData$NANTIfte * changeFromEnrollBase  
        stateData$PBIfte <- stateData$PBIfte * changeFromEnrollBase  
        stateData$TCUfte <- stateData$TCUfte * changeFromEnrollBase  
        stateData$EFYTOTLT <- stateData$EFYTOTLT * changeFromEnrollBase  
        stateData$EFYAAPIT <- stateData$EFYAAPIT * changeFromEnrollBase  
        stateData$EFYAIANT <- stateData$EFYAIANT * changeFromEnrollBase  
        stateData$EFYBKAAT <- stateData$EFYBKAAT * changeFromEnrollBase  
        stateData$EFYHISPT <- stateData$EFYHISPT * changeFromEnrollBase  
        stateData$EFYWHITT <- stateData$EFYWHITT * changeFromEnrollBase  
        stateData$EFYTWOTT <- stateData$EFYTWOTT * changeFromEnrollBase  
        stateData$tuitionFTEproduct <- stateData$tuitionFTEproduct * changeFromEnrollBase  

        tribeData$adjResidentFTE <- tribeData$adjResidentFTE * changeFromEnrollBase  
        tribeData$partnershipCost <- tribeData$partnershipCost * changeFromEnrollBase  
        tribeData$AANAPIIfte <- tribeData$AANAPIIfte * changeFromEnrollBase  
        tribeData$ANNHfte <- tribeData$ANNHfte * changeFromEnrollBase  
        tribeData$HBCUfte <- tribeData$HBCUfte * changeFromEnrollBase  
        tribeData$HSIfte <- tribeData$HSIfte * changeFromEnrollBase  
        tribeData$NANTIfte <- tribeData$NANTIfte * changeFromEnrollBase  
        tribeData$PBIfte <- tribeData$PBIfte * changeFromEnrollBase  
        tribeData$TCUfte <- tribeData$TCUfte * changeFromEnrollBase  
        tribeData$EFYTOTLT <- tribeData$EFYTOTLT * changeFromEnrollBase  
        tribeData$EFYAAPIT <- tribeData$EFYAAPIT * changeFromEnrollBase  
        tribeData$EFYAIANT <- tribeData$EFYAIANT * changeFromEnrollBase  
        tribeData$EFYBKAAT <- tribeData$EFYBKAAT * changeFromEnrollBase  
        tribeData$EFYHISPT <- tribeData$EFYHISPT * changeFromEnrollBase  
        tribeData$EFYWHITT <- tribeData$EFYWHITT * changeFromEnrollBase  
        tribeData$EFYTWOTT <- tribeData$EFYTWOTT * changeFromEnrollBase  
        tribeData$tuitionFTEproduct <- tribeData$tuitionFTEproduct * changeFromEnrollBase  
        
        ###############################################################
        #### Next we have to find out the average tuition/fees per ####
        #### FTE for the universe of colleges included in the      ####
        #### model. We can  derive this from the FTE and           ####
        #### total cost variables for states and tribes.           ####
        ###############################################################
        
        avgTuition <- (sum(stateData$partnershipCost) + sum(tribeData$partnershipCost)) / (sum(stateData$adjResidentFTE) + sum(tribeData$adjResidentFTE))
        
        ###############################################################
        #### At this stage, we will apply the filter for states    ####
        #### the user assumes will not participate. stateDataAll   ####
        #### is created to preserve the list of all states, which  ####
        #### is necessary for Table 
        ###############################################################
        
        stateDataAll <- stateData 
        if(input$medicaid=="No"){
            stateData <- stateData %>% filter(STABBR2 %in% c(
                "AL", "FL", "GA", "KS", "MS", "NC", "SC", "SD", "TN", "TX", "WI", "WY"
            )==FALSE)
        }
        
        ###############################################################
        #### Step 2 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### Next, we determine each state's match rate according  ####
        #### to the selections made by the user. I "null" out any  ####
        #### adjustment variables that the user did not select by  ####
        #### imputing NA values. Then, each state's adjustment is  ####
        #### the average of remaining variables. If no variables   ####
        #### were selected, I impute a value of 1, which means no  ####
        #### adjustment gets made to any state's match rate.       ####
        ###############################################################
        
        stateData$TTRPCindex <- 1/stateData$TTRPCindex # We invert this so that poorer states have higher values. 
        stateData$PIPCindex <- 1/stateData$PIPCindex # We invert this so that poorer states have higher values. 
        
        if(("stateWealth" %in% input$adjustmentFactors)==FALSE){
            stateData$TTRPCindex <- rep(NA, nrow(stateData))  
        }
        if(("childPoverty" %in% input$adjustmentFactors)==FALSE){
            stateData$CPRindex <- rep(NA, nrow(stateData))  
        }
        if(("personalIncome" %in% input$adjustmentFactors)==FALSE){
            stateData$PIPCindex <- rep(NA, nrow(stateData))  
        }
        if(("FRPLshare" %in% input$adjustmentFactors)==FALSE){
            stateData$FRPLindex <- rep(NA, nrow(stateData))  
        }
        if(("pellShare" %in% input$adjustmentFactors)==FALSE){
            stateData$PELLindex <- rep(NA, nrow(stateData))  
        }
        stateData$newIndex <- rowMeans(data.frame(
                                        stateData$TTRPCindex, 
                                        stateData$CPRindex, 
                                        stateData$PIPCindex, 
                                        stateData$FRPLindex,
                                        stateData$PELLindex), 
                                       na.rm=TRUE)
        stateData$newIndex <- ifelse(is.nan(stateData$newIndex), 1, stateData$newIndex)
        
        ###############################################################
        #### Next is the match rate formula. First, we define the  ####
        #### match rate as the average match, as specified by the  ####
        #### user, multiplied by the state's index to the power of ####
        #### the coefficient specified by the user. (A coefficient ####
        #### of zero results in no adjustments.)                   ####
        ###############################################################
        
        stateData$matchRate <- (input$averageMatch / 100) * (stateData$newIndex ^ input$powerCoefficient)
        
        ###############################################################
        #### For any state has a match rate above the maximum set  ####
        #### in the advanced inputs, the maximum rate is imputed.  ####
        ###############################################################
        
        if(input$bounds=="Yes"){
            stateData$matchRate <- ifelse(stateData$matchRate < (input$minmaxFS[1] / 100), (input$minmaxFS[1] / 100), stateData$matchRate)
            stateData$matchRate <- ifelse(stateData$matchRate > (input$minmaxFS[2] / 100), (input$minmaxFS[2] / 100), stateData$matchRate)
        }
        
        ###############################################################
        #### Step 3 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### The incoming federal funding to each state is the     ####
        #### product of resident FTEs, average tuition, and the    ####
        #### state's match rate. The product of FTEs and average   ####
        #### tuition is already stored as partnershipCost, so we   ####
        #### just multiply partnershipCost by matchRate.           ####
        ###############################################################
        
        stateData$federalAllocation <- stateData$partnershipCost * stateData$matchRate
        stateData$federalAllocationPerFTE <- stateData$federalAllocation / stateData$adjResidentFTE
        stateData$federalAllocationPerCapita <- stateData$federalAllocation / stateData$RESall
        
        ###############################################################
        #### Here we make a graph of federalAllocationPerCapita.   ####
        ###############################################################
        
        forChart01 <- stateData %>% select(STABBR2, federalAllocationPerCapita)
        forChart01$federalAllocationPerCapita <- as.numeric(formatC(round(forChart01$federalAllocationPerCapita, 0), format="d", big.mark=","))
        chart01 <- plot_ly(forChart01, x = ~STABBR2, y = ~federalAllocationPerCapita, type = 'bar') %>% layout(title = "New federal funding per resident aged 18-35 without a college degree (FY 2022-23)",
                              xaxis = list(title = "State"),
                              yaxis = list(title = "Federal funding per capita", tickformat = "$"))
        output$chart1 <- renderPlotly({
            chart01
        })
        
        ###############################################################
        #### Step 4 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### Now we can start calculating what the state needs to  ####
        #### contribute to participate.                            ####
        ###############################################################
        
        # stateContribution = The cost of admission. ED will not approve the state's application unless this contribution is provided. 
        stateData$stateContribution <- stateData$partnershipCost * (1 - stateData$matchRate)
        
        # stateNewMoney = New appropriations. Assuming that the state continues to provide state financial aid to community college students in the form of non-tuition stipends, it only needs to newly appropriate stateNewMoney.   
        if(input$californiaPromiseGrant=="Yes"){
            stateData$STATEGRANTS2[stateData$STABBR2=="CA"] <- (stateData$STATEGRANTS2[stateData$STABBR2=="CA"] + 722248598)
        }
        stateData$stateNewMoney <- stateData$stateContribution - stateData$STATEGRANTS2
        stateData$stateNewMoney <- ifelse(stateData$stateNewMoney < 0, 0, stateData$stateNewMoney)
        
        # stateRemainder = The difference between the state's ACP total funds and the actual amount needed to waive tuition. When this amount is positive, the state has to appropriate even more to waive tuition. When this amount is negative, then the state actually has some overflow (see below). 
        stateData$stateRemainder <- stateData$tuitionFTEproduct - (stateData$federalAllocation + stateData$stateNewMoney)
        stateData$extraContribution <- ifelse(stateData$stateRemainder >= 0, abs(stateData$stateRemainder), 0)
        
        # overflow = Money the state has left over to apply to other post-secondary purposes.  
        stateData$overflow <- ifelse(stateData$stateRemainder < 0, abs(stateData$stateRemainder), 0)
        
        stateData$allNewMoney <- stateData$stateNewMoney + stateData$extraContribution
        stateData$appropsIncrease <- (stateData$allNewMoney / stateData$TwoYearEdApprops)
        
        forTable09 <- stateData %>% select(STABBR2, matchRate, federalAllocation, stateNewMoney, extraContribution, allNewMoney, overflow, appropsIncrease)
        forTable10 <- stateData %>% select(STABBR2, matchRate, federalAllocation, stateNewMoney, extraContribution, allNewMoney, overflow, TwoYearEdApprops)
        
        forTable09$matchRate <- label_percent(accuracy=0.1)(round(forTable09$matchRate, 3))
        forTable09$federalAllocation <- dollar(forTable09$federalAllocation)
        forTable09$stateNewMoney <- dollar(forTable09$stateNewMoney)
        forTable09$extraContribution <- dollar(forTable09$extraContribution)
        forTable09$allNewMoney <- dollar(forTable09$allNewMoney)
        forTable09$overflow <- dollar(forTable09$overflow)
        forTable09$appropsIncrease <- label_percent(accuracy=0.1)(round(forTable09$appropsIncrease, 3))

        names(forTable09) <- c("State", "Federal share", "Federal funding", "State participation costs", "State's cost of waiving tuition and fees atop participation costs", "Total new state funding required", "Overflow (see note below)", "Required increase in state's two-year educational appropriations")
        
        output$table9 <- renderDataTable({
            DT::datatable(forTable09, options=list(pagelength=10))
        })

        ###############################################################
        #### We can now sum up those amounts for a summary table.  ####
        ###############################################################
        
        table10 <- data.frame(c("Total federal funding to states", 
                                #"Total state participation costs", 
                                #"Total costs of waiving tuition and fees atop participation costs", 
                                "Total new state funding required", 
                                "Total overflow for other postsecondary purposes", 
                                "State fin. aid for CC tuition available for non-tuition aid", 
                                "Required increase in states' two-year educational appropriations"), 
                              c(dollar(sum(forTable10$federalAllocation)), 
                                #dollar(sum(forTable10$stateNewMoney)), 
                                #dollar(sum(forTable10$extraContribution)), 
                                dollar(sum(forTable10$allNewMoney)), 
                                dollar(sum(forTable10$overflow)), 
                                dollar(sum(stateData$STATEGRANTS2)), 
                                label_percent(accuracy=0.1)(round(
                                    (sum(forTable10$allNewMoney, na.rm=TRUE) / sum(forTable10$TwoYearEdApprops, na.rm=TRUE)), 
                                    3))))
        names(table10) <- c("Measure", "Amount")
        output$table010 <- renderTable({
            table10
        })
        
        ###############################################################
        #### Step 5 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### The ACP bill specifies that tribes get either the     ####
        #### amount of federal funding that they would receive     ####
        #### were they a state, or 95% of what it would take to    ####
        #### eliminate tuition and fees at their own institution   ####
        #### (whichever amount is less). Here we make that         ####
        #### determination for each tribe.                         ####
        ###############################################################
        
        tribeData$costCalculation1 <- tribeData$adjResidentFTE * avgTuition * (input$averageMatch / 100)
        tribeData$costCalculation2 <- tribeData$tuitionFTEproduct * 0.95
        
        tribeData$federalAllocation <- ifelse(tribeData$costCalculation1 < tribeData$costCalculation2, tribeData$costCalculation1, tribeData$costCalculation2)
        tribeData$federalAllocationPerFTE <- tribeData$federalAllocation / tribeData$adjResidentFTE

        ###############################################################
        #### Now, let's create table1, which focuses on the        ####
        #### state-level and tribe-level basics.                   ####
        ###############################################################
        
        table1state <- stateData %>% select(STABBR2, adjResidentFTE, EFYTOTLT, countingVar, matchRate, federalAllocation, federalAllocationPerFTE)
        table1tribe <- tribeData %>% select(STABBR2, adjResidentFTE, EFYTOTLT, countingVar, federalAllocation, federalAllocationPerFTE)
        
        ###############################################################
        #### These next lines are just to make the tables more     ####
        #### easily read.                                          ####
        ###############################################################
        
        table1state$adjResidentFTE <- formatC(round(table1state$adjResidentFTE, 0), format="d", big.mark=",")
        table1state$EFYTOTLT <- formatC(round(table1state$EFYTOTLT, 0), format="d", big.mark=",")
        table1state$matchRate <- label_percent(accuracy=0.1)(round(table1state$matchRate, 3))
        table1state$federalAllocation <- dollar(table1state$federalAllocation)
        table1state$federalAllocationPerFTE <- dollar(table1state$federalAllocationPerFTE, largest_with_cents = 1000)
        
        table1tribe$adjResidentFTE <- formatC(round(table1tribe$adjResidentFTE, 0), format="d", big.mark=",")
        table1tribe$EFYTOTLT <- formatC(round(table1tribe$EFYTOTLT, 0), format="d", big.mark=",")
        table1tribe$federalAllocation <- dollar(table1tribe$federalAllocation)
        table1tribe$federalAllocationPerFTE <- dollar(table1tribe$federalAllocationPerFTE, largest_with_cents = 1000)
        
        names(table1state) <- c("State", "Eligible students (FTE)", "Eligible students (headcount)", "Qualifying institutions", "Federal share", "Federal funding", "Federal funding per FTE student")
        names(table1tribe) <- c("Tribe", "Eligible students (FTE)", "Eligible students (headcount)", "Tribal colleges", "Federal funding", "Federal funding per FTE student")
        
        
        output$table1S <- renderDataTable({
            DT::datatable(table1state, options=list(pagelength=10))
        })
        output$table1T <- renderDataTable({
            DT::datatable(table1tribe, options=list(pagelength=10))
        })
        
        ###############################################################
        #### Next we'll create a map of federal funding per FTE    ####
        #### by state.                                             ####
        ###############################################################
        
        forStatesMap <- stateData %>% select(STABBR2, federalAllocationPerFTE)
        names(forStatesMap) <- c("STABBR2", "Federal Funding Per FTE")
        forStatesMap$hover <- paste0(forStatesMap$STABBR2, "\n$", forStatesMap$federalAllocationPerFTE)
        statesMap <- plot_geo(forStatesMap, locationmode='USA-states') %>% add_trace(locations = ~STABBR2, z = ~`Federal Funding Per FTE`, zmin=0, zmax=5000, colorscale='Electric') %>% layout(geo = list(scope = 'usa'), title="Federal funding per FTE by state") %>% colorbar(tickprefix="$")
        
        output$map1 <- renderPlotly({
            statesMap
        })
        
        ###############################################################
        #### The next table provides some basic stats on ACP.      ####
        ###############################################################
        
        table00col1 <- c("Qualifying institutions",
                         "Eligible students (FTE)",
                         "Eligible students (headcount)", 
                         "ACP funding per FTE")
        table00col2ent1 <- as.numeric(sum(stateData$countingVar) + sum(tribeData$countingVar))
        table00col2ent2 <- as.numeric(sum(stateData$adjResidentFTE) + sum(tribeData$adjResidentFTE))
        table00col2ent3 <- as.numeric(sum(stateData$EFYTOTLT) + sum(tribeData$EFYTOTLT))
        table00col2ent4 <- as.numeric(avgTuition)

        table00col2ent1 <- formatC(round(table00col2ent1, 0), format="d", big.mark=",")
        table00col2ent2 <- formatC(round(table00col2ent2, 0), format="d", big.mark=",")
        table00col2ent3 <- formatC(round(table00col2ent3, 0), format="d", big.mark=",")
        table00col2ent4 <- dollar(table00col2ent4, largest_with_cents = 1000)
        
        table00 <- data.frame(table00col1, c(table00col2ent1, 
                                             table00col2ent2, 
                                             table00col2ent3,
                                             table00col2ent4))
        names(table00) <- c("Measure", "Total")
        
        output$table0 <- renderTable({
            table00
        })
        
        ###############################################################
        #### We now chart out how much the partnership will cost   ####
        #### the federal government over ten years.                ####
        ###############################################################
        
        CPIgrowthRate <- (input$averageCPIgrowth / 100) + 1
        costIn2022 <- sum(stateData$federalAllocation)+sum(tribeData$federalAllocation)+input$blockGrants
        costOverTenYears <- sum(costIn2022 * (CPIgrowthRate)^0, # 2022-23
                                costIn2022 * (CPIgrowthRate)^1, # 2023-24
                                costIn2022 * (CPIgrowthRate)^2, # 2024-25
                                costIn2022 * (CPIgrowthRate)^3, # 2025-26
                                costIn2022 * (CPIgrowthRate)^4, # 2026-27
                                costIn2022 * (CPIgrowthRate)^5, # 2027-28
                                costIn2022 * (CPIgrowthRate)^6, # 2028-29
                                costIn2022 * (CPIgrowthRate)^7, # 2029-30
                                costIn2022 * (CPIgrowthRate)^8, # 2030-31
                                costIn2022 * (CPIgrowthRate)^9  # 2031-32
                                )
        table2fed <- data.frame(timeline = c("First Year (2022-23)", 
                                  "Ten Years (2022-23 to 2031-32)"),
                                cost = c(costIn2022, 
                                  costOverTenYears))
        table2fed$cost <- dollar(table2fed$cost)
        names(table2fed) <- c("Timeframe", "Federal funding")
        output$table2 <- renderTable({
            table2fed
        })
    
        ###############################################################
        #### Step 6 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### Now we determine how differences by state affect how  ####
        #### funding is received by different subgroups. Due to    ####
        #### data limitations, we only focus here on the federal   ####
        #### money to states, ignoring the federal money to tribes.#### 
        ###############################################################
        
        forMergeState <- stateData %>% select(STABBR2, 
                                              federalAllocation, 
                                              EFYTOTLT, 
                                              EFYWHITT, 
                                              EFYBKAAT, 
                                              EFYHISPT, 
                                              EFYAAPIT, 
                                              EFYAIANT, 
                                              EFYTWOTT)
        forMergeTribe <- tribeData %>% select(STABBR2, 
                                              federalAllocation, 
                                              EFYTOTLT, 
                                              EFYWHITT, 
                                              EFYBKAAT, 
                                              EFYHISPT, 
                                              EFYAAPIT, 
                                              EFYAIANT, 
                                              EFYTWOTT)
        mergedData <- rbind(forMergeState, forMergeTribe)
        
        mergedData$fundingPerCCstudent <- mergedData$federalAllocation / mergedData$EFYTOTLT
        
        mergedData$fundingToCCwhite <- mergedData$fundingPerCCstudent * mergedData$EFYWHITT
        mergedData$fundingToCCblack <- mergedData$fundingPerCCstudent * mergedData$EFYBKAAT
        mergedData$fundingToCChispanic <- mergedData$fundingPerCCstudent * mergedData$EFYHISPT
        mergedData$fundingToCCaapi <- mergedData$fundingPerCCstudent * mergedData$EFYAAPIT
        mergedData$fundingToCCnative <- mergedData$fundingPerCCstudent * mergedData$EFYAIANT
        mergedData$fundingToCCtwo <- mergedData$fundingPerCCstudent * mergedData$EFYTWOTT
        
        fundingPerCCstudentByRace <- c(
          (sum(mergedData$fundingToCCwhite) / (sum(stateDataAll$EFYWHITT) + sum(tribeData$EFYWHITT))),
          (sum(mergedData$fundingToCCblack) / (sum(stateDataAll$EFYBKAAT) + sum(tribeData$EFYBKAAT))), 
          (sum(mergedData$fundingToCChispanic) / (sum(stateDataAll$EFYHISPT) + sum(tribeData$EFYHISPT))), 
          (sum(mergedData$fundingToCCaapi) / (sum(stateDataAll$EFYAAPIT) + sum(tribeData$EFYAAPIT))), 
          (sum(mergedData$fundingToCCnative) / (sum(stateDataAll$EFYAIANT) + sum(tribeData$EFYAIANT))), 
          (sum(mergedData$fundingToCCtwo) / (sum(stateDataAll$EFYTWOTT) + sum(tribeData$EFYTWOTT))))
        
        raceNames <- c("White", "Black", "Hispanic", "Asian-American/Pacific Islander", "Native American or Alaskan Native", "Two or more races / Other")
        fundingPerCCstudentByRace <- dollar(fundingPerCCstudentByRace, largest_with_cents = 1000)
        headcountByRace <- c(sum(mergedData$EFYWHITT, na.rm=TRUE), 
                             sum(mergedData$EFYBKAAT, na.rm=TRUE), 
                             sum(mergedData$EFYHISPT, na.rm=TRUE), 
                             sum(mergedData$EFYAAPIT, na.rm=TRUE), 
                             sum(mergedData$EFYAIANT, na.rm=TRUE), 
                             sum(mergedData$EFYTWOTT, na.rm=TRUE))
        table3A <- data.frame(raceNames, fundingPerCCstudentByRace, headcountByRace)
        table3A$headcountByRace <- formatC(round(table3A$headcountByRace, 0), format="d", big.mark=",")
        names(table3A) <- c("For every community college student who is: ", "...new federal funding in 2022-23 equals:", "Total students served:")
        output$table3A <- renderTable({
            table3A
        })
        
        ###############################################################
        #### Now, the same thing but for residents aged 18-35      ####
        #### without a college degree.                             ####
        ###############################################################
          
        stateData$totalResidents <- stateData$RESwhite + 
            stateData$RESblack + 
            stateData$REShispanic + 
            stateData$RESaapi + 
            stateData$RESnative + 
            stateData$REStwo
        stateData$fundingPerResident <- stateData$federalAllocation / stateData$totalResidents
        
        stateData$fundingToRESwhite <- stateData$fundingPerResident * stateData$RESwhite
        stateData$fundingToRESblack <- stateData$fundingPerResident * stateData$RESblack
        stateData$fundingToREShispanic <- stateData$fundingPerResident * stateData$REShispanic
        stateData$fundingToRESaapi <- stateData$fundingPerResident * stateData$RESaapi
        stateData$fundingToRESnative <- stateData$fundingPerResident * stateData$RESnative
        stateData$fundingToREStwo <- stateData$fundingPerResident * stateData$REStwo
        
        fundingPerResidentByRace <- c((sum(stateData$fundingToRESwhite) / sum(stateDataAll$RESwhite)),
                                       (sum(stateData$fundingToRESblack) / sum(stateDataAll$RESblack)), 
                                       (sum(stateData$fundingToREShispanic) / sum(stateDataAll$REShispanic)), 
                                       (sum(stateData$fundingToRESaapi) / sum(stateDataAll$RESaapi)), 
                                       (sum(stateData$fundingToRESnative) / sum(stateDataAll$RESnative)), 
                                       (sum(stateData$fundingToREStwo) / sum(stateDataAll$REStwo)))
        
        totalResidentsByRace <- c(sum(stateDataAll$RESwhite),
                                  sum(stateDataAll$RESblack), 
                                  sum(stateDataAll$REShispanic), 
                                  sum(stateDataAll$RESaapi), 
                                  sum(stateDataAll$RESnative), 
                                  sum(stateDataAll$REStwo))
        
        raceNames <- c("White", "Black", "Hispanic", "Asian-American/Pacific Islander", "Native American or Alaskan Native", "Two or more races / Other")
        fundingPerResidentByRace <- dollar(fundingPerResidentByRace, largest_with_cents = 100)
        table3B <- data.frame(raceNames, fundingPerResidentByRace, totalResidentsByRace)
        table3B$totalResidentsByRace <- formatC(round(table3B$totalResidentsByRace, 0), format="d", big.mark=",")
        names(table3B) <- c("For every resident 18-35 without a degree who is: ", "...new federal funding in 2022-23 equals:", "Total:")
        output$table3B <- renderTable({
            table3B
        })
        
        ###############################################################
        #### Step 7 of methodology:                                ####
        ###############################################################
        
        ###############################################################
        #### Now we pull total federal allocations and FTEs by     ####
        #### Title III institution type.                           ####
        ###############################################################
        
        stateData$AANAPIIshare <- ifelse(is.na(stateData$AANAPIIfte)==FALSE, (stateData$AANAPIIfte / stateData$adjResidentFTE), 0)
        stateData$ANNHshare <- ifelse(is.na(stateData$ANNHfte)==FALSE, (stateData$ANNHfte / stateData$adjResidentFTE), 0)
        stateData$HBCUshare <- ifelse(is.na(stateData$HBCUfte)==FALSE, (stateData$HBCUfte / stateData$adjResidentFTE), 0)
        stateData$HSIshare <- ifelse(is.na(stateData$HSIfte)==FALSE, (stateData$HSIfte / stateData$adjResidentFTE), 0)
        stateData$NANTIshare <- ifelse(is.na(stateData$NANTIfte)==FALSE, (stateData$NANTIfte / stateData$adjResidentFTE), 0)
        stateData$PBIshare <- ifelse(is.na(stateData$PBIfte)==FALSE, (stateData$PBIfte / stateData$adjResidentFTE), 0)
        
        tribeData$AANAPIIshare <- ifelse(is.na(tribeData$AANAPIIfte)==FALSE, (tribeData$AANAPIIfte / tribeData$adjResidentFTE), 0)
        tribeData$ANNHshare <- ifelse(is.na(tribeData$ANNHfte)==FALSE, (tribeData$ANNHfte / tribeData$adjResidentFTE), 0)

        table4 <- data.frame(titleIIInames = c(
                     "Asian American and Native American Pacific Islander-Serving Institutions", 
                     "Alaska Native and Native Hawaiian Serving Institutions", 
                     "Historically Black Colleges and Universities", 
                     "Hispanic Serving Institutions", 
                     "Native American-Serving Nontribal Institutions",
                     "Predominantly Black Institutions",
                     "Tribal Colleges and Universities"
        ),
        titleIIIamounts = c(
          (sum(stateData$federalAllocation * stateData$AANAPIIshare, na.rm=TRUE) + 
               sum(tribeData$federalAllocation * tribeData$AANAPIIshare, na.rm=TRUE)),
          (sum(stateData$federalAllocation * stateData$ANNHshare, na.rm=TRUE) + 
               sum(tribeData$federalAllocation * tribeData$ANNHshare, na.rm=TRUE)),
          sum(stateData$federalAllocation * stateData$HBCUshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$HSIshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$NANTIshare, na.rm=TRUE),   
          sum(stateData$federalAllocation * stateData$PBIshare, na.rm=TRUE),   
          sum(tribeData$federalAllocation)  
        ), 
        titleIIIftes = c(
            (sum(stateData$AANAPIIfte, na.rm=TRUE) + sum(tribeData$AANAPIIfte, na.rm=TRUE)), 
            (sum(stateData$ANNHfte, na.rm=TRUE) + sum(tribeData$ANNHfte, na.rm=TRUE)), 
            sum(stateData$HBCUfte, na.rm=TRUE), 
            sum(stateData$HSIfte, na.rm=TRUE), 
            sum(stateData$NANTIfte, na.rm=TRUE), 
            sum(stateData$PBIfte, na.rm=TRUE), 
            sum(tribeData$TCUfte, na.rm=TRUE)
        ))
        table4$titleIIIamounts <- dollar(as.numeric(table4$titleIIIamounts))
        table4$titleIIIftes <- formatC(round(table4$titleIIIftes, 0), format="d", big.mark=",")
        names(table4) <- c("Title III Institutions", "Total federal allocations", "Total FTEs covered")
        output$table4 <- renderTable({
            table4
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
