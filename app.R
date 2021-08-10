
library(shiny)
library(dplyr)
library(scales)
library(plotly)
library(DT)

ui <- fluidPage(

    titlePanel("America's College Promise Federal-State Partnership"),
    h5("In this app, you can select key design inputs for the federal-state partnership for tuition-free community college and see effects on new federal funding, the funding distribution to states and tribes, and breakdowns by subgroups. This model is based on the America's College Promise Act of 2021 (Baldwin/Levin). To share questions or feedback, please email granville@tcf.org."),

    sidebarLayout(
        sidebarPanel(
            h5(em("Policy Design Choices: Defining Eligible Institutions")),
            selectInput(
                    inputId = "associateThreshold", 
                    label = "For an institution to count as a community college, what is the minimum percentage of degrees awarded that must be at the associate level?", 
                    choices=c(
                        "50%", 
                        "60%", 
                        "70%", 
                        "80%", 
                        "90%", 
                        "95%")
            ),
            h6("Selecting a higher threshold will remove some campuses from the partnership model."),
            selectInput(
                inputId = "graduateFilter", 
                label = "If an institution offers graduate programs, should it be ineligible for the partnership?", 
                choices=c(
                    "Yes", 
                    "No")
            ), 
            h6("If you select 'Yes', a small number of campuses will be removed from the partnership model."),
            h5(em("Policy Design Choices: Federal Share of Costs")),
            sliderInput(
                inputId="averageMatch", 
                label="What should be the base federal match rate to states?",
                min=50, 
                max=100, 
                value=75,
                step=1, 
                post="%"
            ),
            h6("This determines the federal government's share of total costs. If you select no adjustment factors below, then this selected value is the match rate for all states. If you select adjustment factors below, these factors will raise a state's match above or below your selected base rate."),
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
            h6("This value goes into a match rate formula, similar to the formula used for Medicaid funding. A value of 0 gives your selected factors no weight. The higher the value, the more each state's match rate will be determined by these factors."),
            submitButton(text = "Enter my selections", icon = NULL, width = NULL)
        ),

        mainPanel(
           plotlyOutput("map1"),
           # dataTableOutput("table1A"),
           h4("New federal funding for community colleges:"),
           tableOutput("table2"),
           h4("Federal allocations to states: Fiscal Year 2022-23"),
           dataTableOutput("table1S"), 
           h4("Federal allocations to tribes: Fiscal Year 2022-23"),
           dataTableOutput("table1T"),
           h4("New federal funding by race/ethnicity: Fiscal Year 2022-23"),
           tableOutput("table3A"), 
           tableOutput("table3B"), 
           h4("New federal funding by Title III MSI category: Fiscal Year 2022-23"), 
           tableOutput("table4"),
           h6("Note: Neither Alaska nor the District of Columbia have community colleges. The Inupiat Community of the Arctic Slope resides in Alaska and receives a tribal allocation.")
           )
    )
)

server <- function(input, output) {

    observe({
        
        ###############################################################
        #### First we determine which state/tribal files to use    ####
        #### based on the inputs submitted by the user.            ####
        ###############################################################
        
        if(input$associateThreshold=="50%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel50A.csv"
            tribeFile <- "Data/tribeModel50A.csv"
        }
        if(input$associateThreshold=="50%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel50B.csv"
            tribeFile <- "Data/tribeModel50B.csv"
        }
        if(input$associateThreshold=="60%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel60A.csv"
            tribeFile <- "Data/tribeModel60A.csv"
        }
        if(input$associateThreshold=="60%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel60B.csv"
            tribeFile <- "Data/tribeModel60B.csv"
        }
        if(input$associateThreshold=="70%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel70A.csv"
            tribeFile <- "Data/tribeModel70A.csv"
        }
        if(input$associateThreshold=="70%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel70B.csv"
            tribeFile <- "Data/tribeModel70B.csv"
        }
        if(input$associateThreshold=="80%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel80A.csv"
            tribeFile <- "Data/tribeModel80A.csv"
        }
        if(input$associateThreshold=="80%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel80B.csv"
            tribeFile <- "Data/tribeModel80B.csv"
        }
        if(input$associateThreshold=="90%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel90A.csv"
            tribeFile <- "Data/tribeModel90A.csv"
        }
        if(input$associateThreshold=="90%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel90B.csv"
            tribeFile <- "Data/tribeModel90B.csv"
        }
        if(input$associateThreshold=="95%" & input$graduateFilter=="No"){
            stateFile <- "Data/stateModel95A.csv"
            tribeFile <- "Data/tribeModel95A.csv"
        }
        if(input$associateThreshold=="95%" & input$graduateFilter=="Yes"){
            stateFile <- "Data/stateModel95B.csv"
            tribeFile <- "Data/tribeModel95B.csv"
        }
        stateData <- read.csv(stateFile, header=TRUE)
        tribeData <- read.csv(tribeFile, header=TRUE)
        
        ###############################################################
        #### Next we have to find out the average tuition/fees per ####
        #### FTE for the universe of colleges included in the      ####
        #### model. We can actually derive this from the FTE and   ####
        #### total cost variables for states and tribes.           ####
        ###############################################################
        
        avgTuition <- (sum(stateData$partnershipCost) + sum(tribeData$partnershipCost)) / (sum(stateData$adjResidentFTE) + sum(tribeData$adjResidentFTE))
        
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
        #### For any state has a match rate above 100%, I impute   ####
        #### a value of 100%, placing a cap on the match.          ####
        ###############################################################
        
        stateData$matchRate <- ifelse(stateData$matchRate > 1, 1, stateData$matchRate)
        
        ###############################################################
        #### The incoming federal funding to each state is the     ####
        #### product of resident FTEs, average tuition, and the    ####
        #### state's match rate. The product of FTEs and average   ####
        #### tuition is already stored as partnershipCost, so we   ####
        #### just multiply partnershipCost by matchRate.           ####
        ###############################################################
        
        stateData$federalAllocation <- stateData$partnershipCost * stateData$matchRate
        stateData$federalAllocationPerFTE <- stateData$federalAllocation / stateData$adjResidentFTE
        
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
        
        table1state <- stateData %>% select(STABBR2, adjResidentFTE, matchRate, federalAllocation, federalAllocationPerFTE)
        table1tribe <- tribeData %>% select(STABBR2, adjResidentFTE, federalAllocation, federalAllocationPerFTE)
        
        ###############################################################
        #### These next lines are just to make the tables more     ####
        #### easily read.                                          ####
        ###############################################################
        
        table1state$adjResidentFTE <- formatC(round(table1state$adjResidentFTE, 0), format="d", big.mark=",")
        table1state$matchRate <- label_percent(accuracy=0.1)(round(table1state$matchRate, 3))
        table1state$federalAllocation <- dollar(table1state$federalAllocation)
        table1state$federalAllocationPerFTE <- dollar(table1state$federalAllocationPerFTE)
        
        table1tribe$adjResidentFTE <- formatC(round(table1tribe$adjResidentFTE, 0), format="d", big.mark=",")
        table1tribe$federalAllocation <- dollar(table1tribe$federalAllocation)
        table1tribe$federalAllocationPerFTE <- dollar(table1tribe$federalAllocationPerFTE)
        
        names(table1state) <- c("State", "Eligible FTEs", "Federal share", "Federal funding", "Federal funding per FTE")
        names(table1tribe) <- c("Tribe", "Eligible FTEs", "Federal funding", "Federal funding per FTE")
        
        
        output$table1S <- renderDataTable({
            DT::datatable(table1state, options=list(pagelength=10))
        })
        output$table1T <- renderDataTable({
            DT::datatable(table1tribe, options=list(pagelength=10))
        })
        
        ###############################################################
        #### Let's also make one table that sums for all states    ####
        #### and tribes.                                           ####
        ###############################################################
        
        table1all <- data.frame(group = c("States", "Tribes"), 
                              totalFunding = c(sum(table1state$federalAllocation), 
                                               sum(table1tribe$federalAllocation)), 
                              FTEseats = c(sum(table1state$adjResidentFTE), 
                                           sum(table1tribe$adjResidentFTE)))
        names(table1all) <- c("Group", "Total federal funding (2022-23)", "Total eligible FTEs (2022-23)")
        output$table1A <- renderDataTable({
            DT::datatable(table1all)
        })
        
        ###############################################################
        #### Next we'll create a map of federal funding per FTE    ####
        #### by state.                                             ####
        ###############################################################
        
        forStatesMap <- stateData %>% select(STABBR2, federalAllocationPerFTE)
        forStatesMap$hover <- paste0(forStatesMap$STABBR2, "\n$", forStatesMap$federalAllocationPerFTE)
        statesMap <- plot_geo(forStatesMap, locationmode='USA-states') %>% add_trace(locations = ~STABBR2, z = ~federalAllocationPerFTE, zmin=0, zmax=5000, colorscale='Electric') %>% layout(geo = list(scope = 'usa'), title="Federal funding per FTE by state") %>% colorbar(tickprefix="$")
        
        output$map1 <- renderPlotly({
            statesMap
        })
        
        ###############################################################
        #### We now chart out how much the partnership will cost   ####
        #### the federal government over ten years. We assume a 3% ####
        #### increase in costs every year.                         ####
        ###############################################################
        
        costIn2022 <- sum(stateData$federalAllocation)+sum(tribeData$federalAllocation)
        costOverTenYears <- sum(costIn2022 * (1.03)^0, # 2022-23
                                costIn2022 * (1.03)^1, # 2023-24
                                costIn2022 * (1.03)^2, # 2024-25
                                costIn2022 * (1.03)^3, # 2025-26
                                costIn2022 * (1.03)^4, # 2026-27
                                costIn2022 * (1.03)^5, # 2027-28
                                costIn2022 * (1.03)^6, # 2028-29
                                costIn2022 * (1.03)^7, # 2029-30
                                costIn2022 * (1.03)^8, # 2030-31 
                                costIn2022 * (1.03)^9  # 2031-32
                                )
        table2fed <- data.frame(timeline = c("First Year (2022-23)", 
                                  "Ten Years (2022-23 to 2031-32)"),
                                cost = c(costIn2022, 
                                  costOverTenYears))
        table2fed$cost <- dollar(table2fed$cost)
        names(table2fed) <- c("Timeframe", "Federal cost")
        output$table2 <- renderTable({
            table2fed
        })
    
        ###############################################################
        #### Now we determine how differences by state affect how  ####
        #### funding is received by different subgroups. Due to    ####
        #### data limitations, we only focus here on the federal   ####
        #### money to states, ignoring the federal money to tribes.#### 
        ###############################################################
        
        stateData$totalHSstudents <- stateData$HSwhite + 
                                     stateData$HSblack + 
                                     stateData$HShispanic + 
                                     stateData$HSaapi + 
                                     stateData$HSnative + 
                                     stateData$HStwo
        stateData$fundingPerHSstudent <- stateData$federalAllocation / stateData$totalHSstudents
        
        stateData$fundingToHSwhite <- stateData$fundingPerHSstudent * stateData$HSwhite
        stateData$fundingToHSblack <- stateData$fundingPerHSstudent * stateData$HSblack
        stateData$fundingToHShispanic <- stateData$fundingPerHSstudent * stateData$HShispanic
        stateData$fundingToHSaapi <- stateData$fundingPerHSstudent * stateData$HSaapi
        stateData$fundingToHSnative <- stateData$fundingPerHSstudent * stateData$HSnative
        stateData$fundingToHStwo <- stateData$fundingPerHSstudent * stateData$HStwo
        
        fundingPerHSstudentByRace <- c((sum(stateData$fundingToHSwhite) / sum(stateData$HSwhite)),
          (sum(stateData$fundingToHSblack) / sum(stateData$HSblack)), 
          (sum(stateData$fundingToHShispanic) / sum(stateData$HShispanic)), 
          (sum(stateData$fundingToHSaapi) / sum(stateData$HSaapi)), 
          (sum(stateData$fundingToHSnative) / sum(stateData$HSnative)), 
          (sum(stateData$fundingToHStwo) / sum(stateData$HStwo)))
        
        raceNames <- c("White", "Black", "Hispanic", "Asian-American/Pacific Islander", "Native American or Alaskan Native", "Two or more races / Other")
        fundingPerHSstudentByRace <- dollar(fundingPerHSstudentByRace)
        table3A <- data.frame(raceNames, fundingPerHSstudentByRace)
        names(table3A) <- c("For every high school student who is: ", "...there is this much new federal funding in 2022-23:")
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
        
        fundingPerResidentByRace <- c((sum(stateData$fundingToRESwhite) / sum(stateData$RESwhite)),
                                       (sum(stateData$fundingToRESblack) / sum(stateData$RESblack)), 
                                       (sum(stateData$fundingToREShispanic) / sum(stateData$REShispanic)), 
                                       (sum(stateData$fundingToRESaapi) / sum(stateData$RESaapi)), 
                                       (sum(stateData$fundingToRESnative) / sum(stateData$RESnative)), 
                                       (sum(stateData$fundingToREStwo) / sum(stateData$REStwo)))
        
        raceNames <- c("White", "Black", "Hispanic", "Asian-American/Pacific Islander", "Native American or Alaskan Native", "Two or more races / Other")
        fundingPerResidentByRace <- dollar(fundingPerResidentByRace)
        table3B <- data.frame(raceNames, fundingPerResidentByRace)
        names(table3B) <- c("For every resident 18-35 without a degree who is: ", "...there is this much new federal funding in 2022-23:")
        output$table3B <- renderTable({
            table3B
        })
        
        ###############################################################
        #### Now we pull total federal allocations and FTEs by     ####
        #### Title III institution type.                           ####
        ###############################################################
        
        stateData$AANAPISIshare <- ifelse(is.na(stateData$AANAPISIfte)==FALSE, (stateData$AANAPISIfte / stateData$adjResidentFTE), 0)
        stateData$ANNHshare <- ifelse(is.na(stateData$ANNHfte)==FALSE, (stateData$ANNHfte / stateData$adjResidentFTE), 0)
        stateData$HBCUshare <- ifelse(is.na(stateData$HBCUfte)==FALSE, (stateData$HBCUfte / stateData$adjResidentFTE), 0)
        stateData$HSIshare <- ifelse(is.na(stateData$HSIfte)==FALSE, (stateData$HSIfte / stateData$adjResidentFTE), 0)
        stateData$NASNTIshare <- ifelse(is.na(stateData$NASNTIfte)==FALSE, (stateData$NASNTIfte / stateData$adjResidentFTE), 0)
        stateData$PBIshare <- ifelse(is.na(stateData$PBIfte)==FALSE, (stateData$PBIfte / stateData$adjResidentFTE), 0)
        
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
          sum(stateData$federalAllocation * stateData$AANAPISIshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$ANNHshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$HBCUshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$HSIshare, na.rm=TRUE),
          sum(stateData$federalAllocation * stateData$NASNTIshare, na.rm=TRUE),   
          sum(stateData$federalAllocation * stateData$PBIshare, na.rm=TRUE),   
          sum(tribeData$federalAllocation)  
        ), 
        titleIIIftes = c(
            sum(stateData$AANAPISIfte, na.rm=TRUE), 
            sum(stateData$ANNHfte, na.rm=TRUE), 
            sum(stateData$HBCUfte, na.rm=TRUE), 
            sum(stateData$HSIfte, na.rm=TRUE), 
            sum(stateData$NASNTIfte, na.rm=TRUE), 
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
