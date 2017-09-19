AddTitle <- function(d,title){
  d <- as.data.frame(d)
  for(i in 1:ncol(d)) d[,i] <- as.character(d[,i])
  dtop <- d[1,,drop=F]
  dtop <- rbind(dtop,dtop)
  for(i in 1:ncol(dtop)){
    dtop[1:2,i] <- ""
    dtop[2,i] <- names(d)[i]
  }
  dtop[1,1] <- title
  return(rbind(dtop,d))
}

Table_x1 <- function(data){
  res <- data[!cPernorNB %in% c(
    "Ubesvart","Ukjent"
  ),.(isTB=sum(isActive)),by=.(cPernorNB,cyear)]
  resx <- res[,.(isTB=sum(isTB)),by=cyear]
  resx[,cPernorNB:="Totalt utenlandsfødte m kjent oppholdstid"]
  setcolorder(resx,names(res))
  res <- rbind(res,resx)
  res[,cPernorNB:=factor(cPernorNB,levels=c(
    "Under ett år i Norge",
    "Ett til fire år i Norge",
    "5 år eller mer i Norge",
    "Totalt utenlandsfødte m kjent oppholdstid"
  ))]
  setorder(res,-cyear,cPernorNB)
  setcolorder(res,c("cyear","cPernorNB","isTB"))
  res[,perc:=sum(isTB)/2,by=cyear]
  res[,perc:=paste0(round(isTB/perc*100),"%")]
  res <- AddTitle(res,title="Oppholdstid i Norge før diagnose (der angitt), meldt MSIS")
  setattr(res,"filename","oppholdstid_i_norge.xlsx")
  return(res)
}

Table_x2 <- function(data){
  d <- copy(data)
  d[,newIndik:=as.character(NA)]
  d[cIndikNB %in% c(
    "Arbeid med pasienter eller barn"
  ), newIndik:="Arbeid"]
  
  d[cIndikNB %in% c(
    "Immunsvekkende tilstand/behandling"
  ), newIndik:="Immunesvekkelse"]
  
  d[cIndikNB %in% c(
    "Rutineundersøkelse av innvandrer"
  ), newIndik:="Innvandring"]
  
  d[cIndikNB %in% c(
    "Smitteoppsporing (miljøundersøkelse)"
  ), newIndik:="Smitteoppsporing"]
  
  d[cIndikNB %in% c(
    "Ubesvart",
    "Ukjent"
  ), newIndik:="Ubesvart/ukjent"]
  
  d[is.na(newIndik) & !is.na(cIndikNB), newIndik:="Annet"]
  
  xtabs(~d$newIndik)
  d[,newIndik := factor(newIndik,levels=c(
    "Arbeid",
    "Immunesvekkelse",
    "Innvandring",
    "Smitteoppsporing",
    "Annet",
    "Ubesvart/ukjent"
  ))]
  
  res <- d[,.(isTB=sum(isActive)),by=.(newIndik,cyear)]
  setorder(res,-cyear,newIndik)
  setcolorder(res,c("cyear","newIndik","isTB"))
  res <- AddTitle(res,title="Indikasjon for undersøkelse, tuberkulosetilfeller meldt MSIS")
  setattr(res,"filename","indikasjon_for_undersokelse.xlsx")
  return(res)
}




