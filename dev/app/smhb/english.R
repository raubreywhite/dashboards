output$headerAboutYou <- renderText("About you")
output$headerAboutTheProject <- renderText("About the project")
output$headerNeeds <- renderText("Your needs")

output$name <- renderUI({
  textInput("name", "Name", value = "", width = '400', placeholder = NULL)
})

output$email <- renderUI({
  textInput("email", "Email", value = "", width = '400', placeholder = NULL)
})

output$phone <- renderUI({
  textInput("phone", "Phone number", value = "", width = '400', placeholder = NULL)
})

output$projectDepartment <- renderUI({
  textInput("projectDepartment", "Department (4 letters)", value = "", width = '400', placeholder = NULL)
})

output$projectTitle <- renderUI({
  textInput("projectTitle", "Title", value = "", width = '400', placeholder = NULL)
})

output$projectSupervisor <- renderUI({
  textInput("projectSupervisor", "Leader", value = "", width = '400', placeholder = "Name, title, affiliation")
})

output$projectTimeSensitive <- renderUI({
  selectInput("projectTimeSensitive", "Time sensitive", choices = list("No"="No","Yes"="Yes"), width = '400')
})

output$projectDuration <- renderUI({
  selectInput("projectDuration", "Duration (months)", choices = as.list(monthsProjectDuration), width = '400')
})

output$projectStatus <- renderUI({
  radioButtons("projectStatus", "Status", list(
    "Planning stage"="Planning",
    "Data collection stage"="Data collection",
    "Data analysis stage"="Analysis",
    "Publication stage"="Publication"
  ), selected = NULL, inline = F,
  width = '400')
})

output$projectDescription <- renderUI({
  textAreaInput("projectDescription", "Description", value = "", width = 500, height=250, resize="both", placeholder = "1) Object of project. 2) Data (source, number of pt, type of outcome). 3) Methods you want to use. Max 250 words.")
})

output$projectType <- renderUI({
  radioButtons("projectType", "What do you need help with?", list(
    "Bioinformatics"="Bioinformatics",
    "Statistics"="Statistics",
    "Infectious disease modelling"="Modelling",
    "Health economics"="Health economics",
    "Project design and implementation of epidemiological studies"="Projects",
    "Recruitment and questionnaire design"="Questionnaires/recruitment",
    "Automatic graphs and tables for reports"="Automatic reports",
    "Other"="Other"
  ), selected = NULL, inline = F,
  width = '450')
})

output$projectTypeOther <- renderUI({
  textAreaInput("projectTypeOther", "", value = "", width = 500, height=150, resize="both", placeholder = "")
})

output$projectCategory <- renderUI({
  radioButtons("projectCategory", "Type of help", list(
    "Consulting"="Consulting",
    "Collaborating - SMHB does not run analyses"="Collab, not running",
    "Collaborating - SMHB runs analyses"="Collab, running",
    "SMHB project"="SMHB project",
    "Don't know"="Don't know"
  ), selected = NULL, inline = F,
  width = '450')
})

output$responsible1 <- renderUI({
  selectInput("responsible1", "Responsible", choices=as.list(responsible), selected="Unassigned",  width = '400')
})

shinyjs::disable("projectTypeOther")
