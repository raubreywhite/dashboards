output$headerAboutYou <- renderText("Om deg")
output$headerAboutTheProject <- renderText("Om prosjektet")
output$headerNeeds <- renderText("Dine behov")

output$name <- renderUI({textInput("name", "Navn", value = "", width = '400', placeholder = NULL)})
output$email <- renderUI({textInput("email", "Epost", value = "", width = '400', placeholder = NULL)})
output$phone <- renderUI({textInput("phone", "Telefonnummer", value = "", width = '400', placeholder = NULL)})
output$projectDepartment <- renderUI({textInput("projectDepartment", "Avdeling (4 bokstaver)", value = "", width = '400', placeholder = NULL)})
output$projectTitle <- renderUI({textInput("projectTitle", "Tittel", value = "", width = '400', placeholder = NULL)})
output$projectSupervisor <- renderUI({textInput("projectSupervisor", "Leder", value = "", width = '400', placeholder = "Navn, tittel")})
output$projectTimeSensitive <- renderUI({selectInput("projectTimeSensitive", "Tidsensitiv (f.eks utbrudd)", choices = list("Nei"="No","Ja"="Yes"), width = '400')})
output$projectDuration <- renderUI({selectInput("projectDuration", "Lengde (mnd)", choices = as.list(monthsProjectDuration), width = '400')})
output$projectStatus <- renderUI({
  radioButtons("projectStatus", "Status", list(
    "Planlegging"="Planning",
    "Datainnsamling"="Data collection",
    "Dataanalyser"="Analysis",
    "Publisering"="Publication"
  ), selected = NULL, inline = F,
  width = '400')})
output$projectDescription <- renderUI({textAreaInput("projectDescription", "Beskrivelse", value = "", width = 500, height=250, resize="both", placeholder = "1) Prosjektets mål. 2) Data (kilde, antall pasienter, utfall type). 3) Hva slags metoder du vil bruke. Maks 250 ord.")})
output$projectType <- renderUI({
  radioButtons("projectType", "Hva trenger du hjelp med?", list(
    "Bioinformatikk"="Bioinformatics",
    "Statistikk"="Statistics",
    "Infeksjonsmodellering"="Modelling",
    "Helseøkonomi"="Health economics",
    "Prosjektdesign og implementering av epidemiologiske studier"="Projects",
    "Rekruttering og design av spørreskjema"="Questionnaires/recruitment",
    "Automatiske grafer og tabeller til rapporter"="Automatic reports",
    "Annen"="Other"
  ), selected = NULL, inline = F,
  width = '450')
})
output$projectTypeOther <- renderUI({textAreaInput("projectTypeOther", "", value = "", width = 500, height=150, resize="both", placeholder = "")})
output$projectCategory <- renderUI({
  radioButtons("projectCategory", "Type hjelp", list(
    "Rådgiving"="Consulting",
    "Samarbeid - SMHB skal ikke kjøre analyser"="Collab, not running",
    "Samarbeid - SMHB skal kjøre analyser"="Collab, running",
    "SMHB prosjekt"="SMHB project",
    "Vet ikke"="Don't know"
  ), selected = NULL, inline = F,
  width = '450')
})

output$responsible1 <- renderUI({
  selectInput("responsible1", "Ansvarlig", choices=as.list(responsible), selected="Unassigned",  width = '400')
})


output$projectTypeOther <- renderUI({textAreaInput("projectTypeOther", "", value = "", width = 500, height=150, resize="both", placeholder = "")})


shinyjs::disable("projectTypeOther")