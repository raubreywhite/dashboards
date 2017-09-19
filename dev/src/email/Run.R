RAWmisc::DashboardInitialiseClean()

assign("LINUXIP", Sys.getenv("LINUXIP"), envir = globalenv())

suppressMessages(library(gmailr))
suppressMessages(library(magrittr))

EmailIPAddress()
