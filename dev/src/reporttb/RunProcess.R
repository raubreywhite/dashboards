suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_reporttb/", export_all=FALSE)
} else {
  library(reporttb)
}


suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(gmailr))

assign("RUN_ALL", TRUE, envir=globalenv())

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="reporttb"
)

print(DashboardFolder("data_raw"))
allRawDataFiles <- list.files(DashboardFolder("data_raw"))
file <- max(allRawDataFiles)
print(file)
if(!is.na(file)){
  x <- readxl::read_excel(file.path(DashboardFolder("data_raw"),file))
  temp <- lapply(x,class)
  classes <- c()
  for(i in 1:length(temp)){
    classes <- c(classes,temp[[i]][1])
  }
  numNotMissing <- apply(x,2,function(x){sum(!is.na(x))})
  newClasses <- classes
  newClasses[numNotMissing==0] <- "character"
  newClasses[newClasses=="POSIXct"] <- "date"
  newClasses[newClasses=="character"] <- "text"

  x <- readxl::read_excel(file.path(DashboardFolder("data_raw"),file),col_names=names(x),col_types=newClasses,skip=1)

  requiredVariables <- c(
    "Paar",
    "Kategori",
    "Fland",
    "Mfland",
    "Ffland",
    "Fverd",
    "Algr",
    "Bofylk",
    "Organ_1",
    "Indik",
    "Herkomst",
    "Pernor",
    "Dyrk_res",
    "Dirluft_res",
    "Rapp_smitteopp",
    "R_res",
    "H_res",
    "P_res",
    "E_res",
    "S_res",
    "Beh_res",
    "IGRA")

  x <- x[,requiredVariables]

  data <- data.table(x)
  data[,Fland:=stringr::str_trim(Fland)]
  data[,Mfland:=stringr::str_trim(Mfland)]
  data[,Ffland:=stringr::str_trim(Ffland)]

  countryMisspelling <- c(
    "Kasahkstan"="Kasakhstan"
  )
  RAWmisc::RecodeDT(data,countryMisspelling,"Fland")
  RAWmisc::RecodeDT(data,countryMisspelling,"Mfland")
  RAWmisc::RecodeDT(data,countryMisspelling,"Ffland")

  if(Sys.getenv("COMPUTER")=="smhb") unlink(file.path(DashboardFolder("data_raw"),allRawDataFiles),force=TRUE)

  ######
  data[,cyear:=Paar]

  ######
  data[!is.na(Kategori), isActive:=1]
  data[Kategori %in% c(
    "Feil diagnose",
    "Forebyggende behandling",
    "Innflyttet under behandling",
    "Vurderes"), isActive:=0]
  xtabs(~Kategori+isActive,data=data, exclude=NULL)
  with(data, table(Kategori, isActive, useNA="always"))

  ######
  data[!is.na(Fland), isForeignBorn:=1]
  data[Fland=="Norge", isForeignBorn:=0]
  with(data, table(Fland, isForeignBorn, useNA="always"))

  ######
  data[!is.na(Mfland), isForeignBornMother:=1]
  data[Mfland=="Norge", isForeignBornMother:=0]
  with(data, table(Mfland, isForeignBornMother, useNA="always"))

  ######
  data[!is.na(Ffland), isForeignBornFather:=1]
  data[Ffland=="Norge", isForeignBornFather:=0]
  with(data, table(Ffland, isForeignBornFather, useNA="always"))

  ######
  data[, isForeignBornParent:=0]
  data[isForeignBornMother==1 | isForeignBornFather==1, isForeignBornParent:=1]
  data[isForeignBornParent==0 & (is.na(isForeignBornMother) | is.na(isForeignBornFather)), isForeignBornParent:=NA]
  
  ######
  data[, isForeignBornParents:=0]
  data[isForeignBornMother==1 & isForeignBornFather==1, isForeignBornParents:=1]
  data[(is.na(isForeignBornMother) | is.na(isForeignBornFather)), isForeignBornParents:=NA]

  ######
  data[, cFverdNB:=as.character(Fverd)]
  data[Fverd %in% c("Nord-Amerika","Sør- og Mellom-Amerika"), cFverdNB:="Amerika"]
  data[Fverd %in% c("Europa"), cFverdNB:="Europa utenfor Norge"]
  data[isForeignBorn==0, cFverdNB:="Norge"]
  with(data, table(Fverd, cFverdNB, useNA="always"))

  ######
  data[, cFlandNB:=as.character(Fland)]

  ######
  data[, cAlgr:=as.character(Algr)]

  ######
  data[, cNorwegianStatusNB := as.character(NA)]
  data[isForeignBorn==0 & isForeignBornParent==0, cNorwegianStatusNB:="Norskfødt med to norskfødte foreldre"]
  data[isForeignBorn==0 & isForeignBornParent==1, cNorwegianStatusNB:="Norskfødt med minst en utenlandsfødt forelder"]
  data[isForeignBorn==1, cNorwegianStatusNB:="Utenlandsfødte"]
  
  ######
  data[, cAltNorwegianStatusNB := as.character(NA)]
  data[isForeignBorn==0 & isForeignBornParents==0, cAltNorwegianStatusNB:="Norskfødt med minst en norskfødte foreldre"]
  data[isForeignBorn==0 & isForeignBornParents==1, cAltNorwegianStatusNB:="Norskfødt med to utenlandsfødt forelder"]
  data[isForeignBorn==1, cAltNorwegianStatusNB:="Utenlandsfødte"]

  ######
  data[, cFylke := as.character(Bofylk)]
  data[Bofylk=="Utenfor Fastlands-Norge", cFylke:="Utenfor fastlandet"]

  ######
  data[, cOrganNB:= Organ_1]
  data[Organ_1 %in% c(
    "Columna",
    "Ben/ledd utenom columna",
    "Columna/ben/ledd"
  ), cOrganNB:="Ben/ledd/columna"]

  data[Organ_1 %in% c(
    "Sentralnervesystem annet enn meninger",
    "Meninger",
    "Meninger/CNS"
  ), cOrganNB:="Sentralnervesystem"]

  data[Organ_1 %in% c(
    "Lymfe/hilusglandler"
  ), cOrganNB:="Lymfeknuter"]

  with(data, table(Organ_1, cOrganNB, useNA="always"))

  ######
  data[, cOrganLungsVsLymphNB := as.character(NA)]
  data[!is.na(cOrganNB), cOrganLungsVsLymphNB:="Alle andre TB tilfeller"]
  data[cOrganNB == "Lunge", cOrganLungsVsLymphNB:="TB i lunger"]
  data[cOrganNB == "Lymfeknuter", cOrganLungsVsLymphNB:="TB i lymfeknuter"]

  ######
  data[, cIndikNB := Indik]
  data[is.na(Indik), cIndikNB:="Ubesvart"]

  ######
  data[, cHerkomstNB := Herkomst]
  data[is.na(Herkomst), cHerkomstNB := "Ubesvart"]

  ######
  data[, cPernorNB := Pernor]
  data[is.na(Pernor), cPernorNB := "Ubesvart"]

  data[Pernor %in% c(
    "Under en måned",
    "1-6 måneder",
    "6-12 måneder"
  ), cPernorNB := "Under ett år i Norge"]

  data[Pernor %in% c(
    "1-2 år",
    "3-4 år"
  ), cPernorNB := "Ett til fire år i Norge"]

  data[Pernor %in% c(
    "5-9 år",
    "10 år eller mer"
  ), cPernorNB := "5 år eller mer i Norge"]

  ######
  data[, cDyrkResAllOrganNB := Dyrk_res]
  data[Dyrk_res %in% c(
    "Ikke utført",
    "Ukjent"
  ) | is.na(Dyrk_res), cDyrkResAllOrganNB := "Ukjent/ikke utført"]

  data[, cDyrkResLungNB := cDyrkResAllOrganNB]
  data[cOrganNB=="Lunge", cDyrkResLungNB := NA]

  ######
  data[, cDirluftAllOrganNB := Dirluft_res]
  data[Dirluft_res %in% c(
    "Ikke utført",
    "Ukjent"
  ) | is.na(Dirluft_res), cDirluftAllOrganNB := "Ukjent/ikke utført"]

  data[, cDirluftLungNB := cDirluftAllOrganNB]
  data[cOrganNB=="Lunge", cDirluftLungNB := NA]

  ######
  data[,isNeedsReport:=0]
  data[cOrganNB=="Lunge" & cDyrkResLungNB=="Positivt", isNeedsReport:=1]

  ######
  data[, Rapp_smitteopp:=gsub(" ", "", Rapp_smitteopp)]
  data[, isReported:=NA]
  data[isNeedsReport==1, isReported:=0]
  data[!is.na(isReported) & Rapp_smitteopp=="Ja", isReported:=1]

  ######
  for(x in c("R","H","P","E","S")){
    uncleaned <- paste0(x,"_res")
    cleaned <- paste0("is",x,"Res")

    txt <- paste0('data[cDyrkResAllOrganNB=="Positivt",',cleaned,':=0]')
    eval(parse(text=txt))

    txt <- paste0('data[!is.na(',cleaned,') & ',uncleaned,'%in% c("Lavgradig resistens","Resistent"),',cleaned,':=1]')
    eval(parse(text=txt))
  }

  ######
  data[cDyrkResAllOrganNB=="Positivt", isMDR:=0]
  data[!is.na(isMDR) & isRRes==1 & isHRes==1, isMDR:=1]

  ######
  data[,cBirthPlaceSovietNB:=cFverdNB]

  data[cFverdNB %in% c(
    "Norge",
    "Europa utenfor Norge"
  ), cBirthPlaceSovietNB:="Andre europeiske land"]

  data[cBirthPlaceSovietNB %in% c(
    "Andre europeiske land"
  ) & cFlandNB %in% c(
    "Armenia",
    "Aserbajdsjan",
    "Belarus",
    "Estland",
    "Georgia",
    "Kasahkstan",
    "Latvia",
    "Litauen",
    "Moldova",
    "Russland",
    "Tadsjikistan",
    "Turkmenistan",
    "Ukraina",
    "Usbekistan"
  ), cBirthPlaceSovietNB:="Tidligere Sovjet"]

  ######
  data[cOrganNB=="Lunge" & Kategori=="Tuberkulose for første gang" & isActive==1, isLungTBFirstTime:=1]

  ######
  data[,cTreatmentResNB:=Beh_res]
  data[is.na(Beh_res), cTreatmentResNB := "Mangler"]
  data[,cTreatmentResNB := trimws(cTreatmentResNB)]

  ######
  data[,cTreatmentResCollapsedNB := cTreatmentResNB]
  data[cTreatmentResNB %in% c(
    "Død av annen årsak",
    "Død av ukjent årsak",
    "Død av tuberkulose",
    "Død tub medvirkende"
  ), cTreatmentResCollapsedNB:="Død"]

  data[cTreatmentResNB %in% c(
    "Forsvunnet fra behandling"
  ), cTreatmentResCollapsedNB:="Forsvunnet"]

  data[cTreatmentResNB %in% c(
    "Reist frivillig ut av landet"
  ), cTreatmentResCollapsedNB:="Reist frivillig"]

  data[cTreatmentResNB %in% c(
    "Bortvist fra landet"
  ), cTreatmentResCollapsedNB:="Bortvist"]

  data[cTreatmentResNB %in% c(
    "MDRTB behandlingsregime"
  ), cTreatmentResCollapsedNB:="Fortsatt under behandling"]

  ######
  data[!is.na(Kategori), isLatentTB:=0]
  data[Kategori=="Forebyggende behandling", isLatentTB:=1]

  ######
  data[isLatentTB==1,cIGRAResultNB:="Ukjent/ubesvart/gråsone/inkonklusiv IGRA"]
  data[isLatentTB==1 & IGRA=="Negativ IGRA",cIGRAResultNB:="Negativ IGRA"]
  data[isLatentTB==1 & IGRA=="Positiv IGRA",cIGRAResultNB:="Positiv IGRA"]

  ######
  cleanedVars <- unique(c(grep("^c",names(data)),grep("^is",names(data))))
  cleanData <- data[,cleanedVars,with=F]


  saveRDS(cleanData,file=file.path(DashboardFolder("data_app"),"data.RDS"))

  if(Sys.getenv("COMPUTER")=="smhb") EmailNotificationOfNewData()
}

x <- cleanData[cNorwegianStatusNB!="Utenlandsfødte",.(isActive=sum(isActive)),by=.(cyear)]
setorder(x,cyear)
x
