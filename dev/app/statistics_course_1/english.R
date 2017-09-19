output$headerTitle <- renderText("Mapping statistical knowledge")
output$headerYourLevel <- renderText("Your level")
output$headerYourWants <- renderText("What do you want?")
output$headerYourData <- renderText("Describe the data you work with")
output$headerOtherComments <- renderText("Further comments?")

output$levelStatistics <- renderUI({
  radioButtons("levelStatistics", "Statistics level", choices = list(
    "None"="None",
    "Beginner"="Beginner",
    "Competent"="Competent",
    "Advanced"="Advanced"
  ), width = '400',inline=F)
})

output$levelR <- renderUI({
  radioButtons("levelR", "R level", choices = list(
    "None"="None",
    "Beginner"="Beginner",
    "Competent"="Competent",
    "Advanced"="Advanced"
  ), width = '400',inline=F)
})

output$levelSTATA <- renderUI({
  radioButtons("levelSTATA", "STATA level", choices = list(
    "None"="None",
    "Beginner"="Beginner",
    "Competent"="Competent",
    "Advanced"="Advanced"
  ), width = '400',inline=F)
})

output$levelSPSS <- renderUI({
  radioButtons("levelSPSS", "SPSS level", choices = list(
    "None"="None",
    "Beginner"="Beginner",
    "Competent"="Competent",
    "Advanced"="Advanced"
  ), width = '400',inline=F)
})

output$preferredLanguage <- renderUI({
  radioButtons("preferredLanguage", "Preferred language", choices = list(
    "R",
    "STATA",
    "SPSS",
    "JMP",
    "Excel"
  ), width = '400',inline=F)
})

output$hoursWorked <- renderUI({
  shiny::selectInput("hoursWorked","Number of hours per week you work with data and/or analyses",
                     choices=as.list(0:40),width=400)
})

output$wantsStatisticsProblems <- renderUI({
  shiny::textAreaInput("wantsStatisticsProblems",
                       "Describe your challenges with running statistical analyses independently (running analyses, interpreting results, choosing methods, etc)",width=400,height=200)
})

output$wantsCoursesMain <- renderUI({
  checkboxGroupInput("wantsCoursesMain", "Preferred courses", choices = list(
    "NOT INTERESTED IN STATISTICS COURSE"="Not interested",
    "Which statistical methods should you choose?"="Which stats methods",
    "Intro to R"="Intro to R",
    "Contingency tables"="Contingency tables",
    "Linear regression"="Linear regression",
    "Logistic regression"="Logistic regression",
    "Longitudinal data (multiple observations per person)"="Longitudinal",
    "Poisson regression (count data)"="Poisson",
    "Sample size calculations"="Sample size",
    "Organising analysis files"="Organising files",
    "Outcome measures (NNT/AAR/AR/OR)"="Outcome measures",
    "Survival analysis"="Survival",
    "Study planning (study types)"="Study planning",
    "Time series"="Time series"
  ), width = '400',inline=F)
})

output$wantsCoursesOther <- renderUI({
  textInput("wantsCoursesOther", "Preferred courses (other)", value = "", width = '400', placeholder = NULL)
})

output$dataSize <- renderUI({
  checkboxGroupInput("dataSize", "Size", choices = list(
    "Under 1000 observations"="Under 1000 obs",
    "Over 1000 observastions"="Over 1000 obs",
    "Over 10.000 observations"="Over 10.000 obs",
    "Over 100.000 observations"="Over 100.000 obs",
    "Under 10 variables"="Under 10 var",
    "Over 10 variables"="Over 10 var",
    "Over 50 variables"="Over 50 var",
    "Over 200 variables"="Over 200 var",
    "Don't know"="Don't know"
    ), width='400', inline=F)
})

output$dataSource <- renderUI({
  checkboxGroupInput("dataSource", "Data source", choices = list(
    "Experiments"="Experiments",
    "Genetic data"="Genetic data",
    "Case-control studies"="Case-control",
    "Clinical data"="Clinical data",
    "Joined datasets"="Joined",
    "Lab data"="Lab data",
    "Randomized clinical trials"="RCT",
    "Person-level register data"="Person-level register data",
    "Person-level surveys"="Person-level surveys",
    "Count data from registers"="Count data from registers",
    "Outbreak data"="Outbreak data",
    "Virology data"="Virology data",
    "Don't know"="Don't know"
  ), width='400', inline=F)
})

output$dataType <- renderUI({
  checkboxGroupInput("dataType", "Type of data", choices = list(
    "Binary outcome"="Binary outcome",
    "Exposure variable from lab"="Exposure variable from lab",
    "Continuous outcome"="Continuous outcome",
    "Clusters"="Clusters",
    "Longitudinal data (multiple observations per person)"="Longitudinal",
    "Time series"="Time series",
    "Outcome variable from lab"="Outcome variable from lab",
    "Censored data"="Censored data",
    "Don't know"="Don't know"
  ), width = '400',inline=F)
})

output$furtherComments <- renderUI({
  shiny::textAreaInput("furtherComments",
                       NULL,width=400,height=200)
})



