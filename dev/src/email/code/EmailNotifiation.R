EmailIPAddress <- function(){
  OAUTHLocation = file.path("/etc", "gmailr", ".httr-oauth")
  
  #emails <- readxl::read_excel("data/emails.xlsx")$data
  #emails <- emails[!is.na(emails) & emails!=""]
  emails <- c("richard.white@fhi.no")
  emailText <- "

------------------------
DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
  
To add or remove people to/from this notification list, send their details to richard.white@fhi.no
  
  "
  
  mime() %>%
    to("dashboards@fhi.no") %>%
    from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    bcc(paste0(emails,collapse=",")) %>%
    subject(paste0("IP: ",LINUXIP)) %>%
    text_body(emailText) -> text_msg
  
  currentWD <- getwd()
  tmp <- tempdir()
  file.copy(OAUTHLocation, paste0(tmp, "/.httr-oauth"))
  setwd(tmp)
  gmailr::gmail_auth()
  gmailr::send_message(text_msg)
  setwd(currentWD)
}


