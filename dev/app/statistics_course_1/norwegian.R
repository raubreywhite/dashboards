output$headerTitle <- renderText("Spørreskjema til kartlegging av statistikkunnskap")
output$headerYourLevel <- renderText("Ditt nivå")
output$headerYourWants <- renderText("Dine ønsker?")
output$headerYourData <- renderText("Beskriv dataene som du jobber med")
output$headerOtherComments <- renderText("Andre kommentarer?")

output$levelStatistics <- renderUI({
  radioButtons("levelStatistics", "Statistikk nivå", choices = list(
    "Ingen"="None",
    "Nybegynner"="Beginner",
    "Kompetent"="Competent",
    "Avansert"="Advanced"
  ), width = '400',inline=F)
})

output$levelR <- renderUI({
  radioButtons("levelR", "R nivå", choices = list(
    "Ingen"="None",
    "Nybegynner"="Beginner",
    "Kompetent"="Competent",
    "Avansert"="Advanced"
  ), width = '400',inline=F)
})

output$levelSTATA <- renderUI({
  radioButtons("levelSTATA", "STATA nivå", choices = list(
    "Ingen"="None",
    "Nybegynner"="Beginner",
    "Kompetent"="Competent",
    "Avansert"="Advanced"
  ), width = '400',inline=F)
})

output$levelSPSS <- renderUI({
  radioButtons("levelSPSS", "SPSS nivå", choices = list(
    "Ingen"="None",
    "Nybegynner"="Beginner",
    "Kompetent"="Competent",
    "Avansert"="Advanced"
  ), width = '400',inline=F)
})

output$preferredLanguage <- renderUI({
  radioButtons("preferredLanguage", "Foretrukket språk", choices = list(
    "R",
    "STATA",
    "SPSS",
    "JMP",
    "Excel"
  ), width = '400',inline=F)
})

output$hoursWorked <- renderUI({
  shiny::selectInput("hoursWorked","Antall timer per uke du jobber med data og/eller analyser",
                     choices=as.list(0:40),width=400)
})

output$wantsStatisticsProblems <- renderUI({
  shiny::textAreaInput("wantsStatisticsProblems",
                       "Utdyp dine utfordringer i forhold til å kjøre statistiske analyser selv (å kjøre analyser, å fortolke resultater, å velge metoder, etc)",width=400,height=200)
})

output$wantsCoursesMain <- renderUI({
  checkboxGroupInput("wantsCoursesMain", "Foretrukket kurs", choices = list(
    "IKKE INTERESSERT I STATISTIKKURS"="Not interested",
    "Hvilke statistiske analyser bør du velge?"="Which stats methods",
    "Intro til R"="Intro to R",
    "Kontingenstabeller"="Contingency tables",
    "Linear regression"="Linear regression",
    "Logistic regression"="Logistic regression",
    "Longitudinal data (multiple observations per person)"="Longitudinal",
    "Poisson regression (count data)"="Poisson",
    "Størrelsesberegninger"="Sample size",
    "Organisere analysefiler"="Organising files",
    "Outcome measures (NNT/AAR/AR/OR)"="Outcome measures",
    "Overlevelsesanalyse"="Survival",
    "Studieplanlegging"="Study planning",
    "Tidsserier"="Time series"
  ), width = '400',inline=F)
})

output$wantsCoursesOther <- renderUI({
  textInput("wantsCoursesOther", "Foretrukket kurs (annet)", value = "", width = '400', placeholder = NULL)
})

output$dataSize <- renderUI({
  checkboxGroupInput("dataSize", "Størrelse", choices = list(
    "Under 1000 observasjoner"="Under 1000 obs",
    "Over 1000 observasjoner"="Over 1000 obs",
    "Over 10.000 observasjoner"="Over 10.000 obs",
    "Over 100.000 observasjoner"="Over 100.000 obs",
    "Under 10 variabler"="Under 10 var",
    "Over 10 variabler"="Over 10 var",
    "Over 50 variabler"="Over 50 var",
    "Over 200 variabler"="Over 200 var",
    "Vet ikke"="Don't know"
    ), width='400', inline=F)
})

output$dataSource <- renderUI({
  checkboxGroupInput("dataSource", "Data kilde", choices = list(
    "Eksperimenter"="Experiments",
    "Genetiske data"="Genetic data",
    "Kasus-kontroll studier"="Case-control",
    "Kliniske data"="Clinical data",
    "Koblete data"="Joined",
    "Lab data"="Lab data",
    "Randomiserte kliniske studier"="RCT",
    "Registerdata på individnivå"="Person-level register data",
    "Spørreskjemaer på individnivå"="Person-level surveys",
    "Tall fra register"="Count data from registers",
    "Utbrudds data"="Outbreak data",
    "Virologidata"="Virology data",
    "Vet ikke"="Don't know"
  ), width='400', inline=F)
})

output$dataType <- renderUI({
  checkboxGroupInput("dataType", "Type data", choices = list(
    "Binært utfall"="Binary outcome",
    "Eksponeringsvariabel fra lab"="Exposure variable from lab",
    "Kontinuerlig utfall"="Continuous outcome",
    "Klynge"="Clusters",
    "Longitudinale"="Longitudinal",
    "Tidsserier"="Time series",
    "Utfall fra lab"="Outcome variable from lab",
    "Sensuerte data"="Censored data",
    "Vet ikke"="Don't know"
  ), width = '400',inline=F)
})


output$furtherComments <- renderUI({
  shiny::textAreaInput("furtherComments",
                       NULL,width=400,height=200)
})



