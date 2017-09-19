palette <- "Set1"


Figure1 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  legend1Options <- list()
  legend1Options[["NB"]] <- c("Norskfødte","Utenlandsfødte","Totalt")
  legend1Options[["EN"]] <- c("Nor. born","Foreign born","All cases")
  
  
  legendOptions <- legend1Options[[language]]
  
  pd1 <- data[,.(isActive=sum(isActive,na.rm=T)),
              by=.(cyear)]
  pd1[,isForeignBorn:=2]
  
  pd2 <- data[,.(isActive=sum(isActive,na.rm=T)),
              by=.(cyear,isForeignBorn)]
  setcolorder(pd2,names(pd1))
  pd <- rbind(pd1,pd2)
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),0:2,stringsAsFactors = FALSE))
  setnames(skeleton, c("cyear","isForeignBorn"))
  pd <- merge(skeleton,pd,by=c("cyear","isForeignBorn"),all.x=TRUE)
  pd[is.na(isForeignBorn), isForeignBorn:=0]
  
  pd[,prettyName:=""]
  pd[isForeignBorn==0,prettyName:=legendOptions[1]]
  pd[isForeignBorn==1,prettyName:=legendOptions[2]]
  pd[isForeignBorn==2,prettyName:=legendOptions[3]]
  pd <- pd[cyear > max(cyear)-35]
  
  graphRange <- diff(range(pd$cyear))
  nudgeX <- graphRange*0.15
  extensionX <- graphRange*0.225
  if(moreSpace){
    nudgeX <- nudgeX*1.5
    extensionX <- extensionX*1.5
  }
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Tuberkulosetilfeller"
    if(relSize<2.5){
      title <- sprintf("Meldte tuberkulosetilfeller etter fødested, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Meldte tuberkulosetilfeller etter fødested, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Tuberculosis cases"
    if(relSize<2.5){
      title <- sprintf("Tuberculosis cases by birthplace, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Tuberculosis cases by birthplace, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  if(type=="a") {
    q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=0.4*relSize,bottomY=0))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    if(relSize<2.5){
      q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    } else {
      q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),2),minor_breaks=NULL)
    }
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + theme(legend.position="bottom")
    q <- q + labs(title=title)
    
  } else {
    pd[,prettyName:=LineBreak(prettyName)]
    q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=0.4*relSize,bottomY=0))
    q <- q + geom_rect(xmin=max(pd$cyear),xmax=Inf,ymin=-Inf,ymax=Inf,fill="white",colour=NA)
    q <- q + geom_line(lwd=2*relSize)
    q <- q +
      directlabels::geom_dl(data=pd,mapping=aes(label = prettyName, fill="black"), method=labelMethodRight)
    q <- q + ThemeShiny(base_size=24*relSize)
    if(relSize<2.5){
      q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    } else {
      q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear),2),minor_breaks=NULL)
    }
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + expand_limits(y=0,x=max(pd$cyear)+extensionX)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
    q <- q + labs(title=title)
  } 
  return(q)
}

Figure2 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  pd <- data[isActive==1,
             .(isActive=sum(isActive,na.rm=T)),
             by=.(cyear,cFlandNB)]
  if(language=="EN") RAWmisc::RecodeDT(pd,RAWmisc::CountriesNBtoEN(),"cFlandNB")
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cFlandNB),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cFlandNB"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cFlandNB"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  pd[cyear==max(cyear),rank:=rank(-isActive)]
  pd[,rank:=mean(rank,na.rm=T),by=cFlandNB]
  

  pd <- pd[cyear > max(cyear)-15 & rank<7]
  
  graphRange <- diff(range(pd$cyear))
  nudgeX <- graphRange*0.1
  extensionX <- graphRange*0.125
  if(moreSpace){
    nudgeX <- nudgeX*2
    extensionX <- extensionX*2
  }
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Tuberkulosetilfeller"
    if(relSize<2.5){
      title <- sprintf("Vanligste fødeland for meldte tuberkulosetilfeller, MSIS %s",max(pd$cyear))
    } else {
      title <- sprintf("Vanligste fødeland for meldte tuberkulosetilfeller, MSIS %s",max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Tuberculosis cases"
    if(relSize<2.5){
      title <- sprintf("Most common birthplaces for tuberculosis cases, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Most common birthplaces for tuberculosis cases,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  if(type=="a") {
    pd[,prettyName:=cFlandNB]
    q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=0.4*relSize,bottomY=0))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + expand_limits(y=0)
    q <- q + labs(title=title)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + theme(legend.position="bottom")
  } else {
    pd[,prettyName:=LineBreak(cFlandNB)]
    q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=0.4*relSize,bottomY=0))
    q <- q + geom_rect(xmin=max(pd$cyear),xmax=Inf,ymin=-Inf,ymax=Inf,fill="white",colour=NA)
    q <- q + geom_line(lwd=2*relSize)
    q <- q +
      directlabels::geom_dl(data=pd,mapping=aes(label = prettyName), method=labelMethodRight)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + expand_limits(y=0,x=max(pd$cyear)+extensionX)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
    q <- q + labs(title=title)
  } 
  return(q)
}



Figure3 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  pd1 <- data[!is.na(cAlgr) & !is.na(cNorwegianStatusNB),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cAlgr,cNorwegianStatusNB,cyear)]
  pd2 <- data[!is.na(cAlgr),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cAlgr,cyear)]
  pd2[,cNorwegianStatusNB:="Totalt"]
  setcolorder(pd2,names(pd1))
  pd <- rbind(pd1,pd2)
  
  switch <- c(
    "Totalt"="Total",
    "Utenlandsfødte"="Foreign born",
    "Norskfødt med to norskfødte foreldre"="Nor. born: 2 Nor. born parents",
    "Norskfødt med minst en utenlandsfødt forelder"="Nor. born: >=1 foreign born parents"
  )
  if(language=="EN") RAWmisc::RecodeDT(pd,switch,"cNorwegianStatusNB")
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cNorwegianStatusNB),unique(pd$cAlgr),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cNorwegianStatusNB","cAlgr"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cNorwegianStatusNB","cAlgr"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  
  pd <- pd[cyear==max(cyear)]
  
  xLabs <- sort(unique(pd$cAlgr))
  xVals <- 1:length(xLabs)
  pd[,x:=as.numeric(factor(cAlgr,levels=xLabs))]
  
  graphRange <- diff(range(pd$x))
  nudgeX <- graphRange*0.15
  if(relSize<2.5){
    extensionX <- graphRange*0.275
  } else {
    extensionX <- graphRange*0.335
  }
  
  if(language=="NB"){
    titleX <- "Alder"
    titleY <- "Tuberkulosetilfeller"
    if(relSize<2.5){
      title <- sprintf("Meldte tuberkulosetilfeller etter fødested og 10 års aldersgrupper, MSIS %s",max(pd$cyear))
    } else {
      title <- sprintf("Meldte tuberkulosetilfelle etter fødested\nog 10 års aldersgrupper, MSIS %s",max(pd$cyear))
    }
  } else {
    titleX <- "Age"
    titleY <- "Tuberculosis cases"
    if(relSize<2.5){
      title <- sprintf("Registered TB cases by birthplace and 10 year age groups, MSIS %s",max(pd$cyear))
    } else {
      title <- sprintf("Registered TB cases by birthplace and 10 year age groups,\nMSIS %s",max(pd$cyear))
    }
  }
  
  if(type=="a") {
    pd[,prettyName:=cNorwegianStatusNB]
    q <- ggplot(pd,aes(x=x,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=-0.4*relSize,bottomY=1))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + scale_x_continuous("",breaks=xVals,labels=xLabs, minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + theme(legend.position="bottom")
    if(relSize>=2.5){
      q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    }
  } else {
    pd[,prettyName:=LineBreak(cNorwegianStatusNB,len=25)]
    
    q <- ggplot(pd,aes(x=x,y=isActive,colour=prettyName,labelSize=1.75*relSize,nudgeX=-0.4*relSize,bottomY=1))
    q <- q + geom_rect(xmin=-Inf,xmax=1,ymin=-Inf,ymax=Inf,fill="white",colour=NA)
    q <- q + geom_line(lwd=2*relSize)
    q <- q +
      directlabels::geom_dl(data=pd,mapping=aes(label = prettyName), method=labelMethodLeft)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + scale_x_continuous(titleX,breaks=xVals,labels=xLabs, minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0,x=min(pd$x)-extensionX)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
  }
  
  return(q)
}

Figure4 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  fylkeLevels <- c(
    "Akershus",
    "Aust-Agder",
    "Buskerud",
    "Finnmark",
    "Hedmark",
    "Hordaland",
    "Møre og Romsdal",
    "Nord-Trøndelag",
    "Nordland",
    "Oppland",
    "Oslo",
    "Rogaland",
    "Sogn og Fjordane",
    "Sør-Trøndelag",
    "Telemark",
    "Troms",
    "Vest-Agder",
    "Vestfold",
    "Østfold",
    "Utenfor fastlandet",
    "Ukjent fylke"
  )
  pd <- data[!is.na(cFylke),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cFylke,cyear)]
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),fylkeLevels,stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cFylke"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cFylke"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  
  if(language=="NB"){
    pd[,prettyName:=sprintf("TB meldt i %s",cyear)]
  } else {
    pd[,prettyName:=sprintf("TB registered %s",cyear)]
  }
  pd <- pd[cyear>max(cyear)-2]
  
  pd[,cFylke:=factor(cFylke, levels = rev(fylkeLevels))]
  pd <- pd[!cFylke %in% c("Utenfor fastlandet","Ukjent fylke")]
  
  if(language=="NB"){
    titleX <- ""
    titleY <- "Tuberkulosetilfeller"
    if(relSize<2.5){
      title <- sprintf("Antall tuberkulosetilfeller meldt etter fylke, MSIS %s og %s", min(pd$cyear), max(pd$cyear))
    } else {
      title <- sprintf("Antall tuberkulosetilfeller meldt etter fylke,\nMSIS %s og %s", min(pd$cyear), max(pd$cyear))
    }
  } else {
    titleX <- ""
    titleY <- "Tuberculosis cases"
    if(relSize<2.5){
      title <- sprintf("Tuberculosis cases by county, MSIS %s and %s", min(pd$cyear), max(pd$cyear))
    } else {
      title <- sprintf("Tuberculosis cases by county,\nMSIS %s and %s", min(pd$cyear), max(pd$cyear))
    }
  }
  
  q <- ggplot(pd,aes(x=cFylke,y=isActive,fill=prettyName,group=prettyName))
  q <- q + geom_bar(stat="identity",position="dodge",width=0.9,alpha=0.7)
  q <- q + geom_text(aes(label=isActive),hjust=-0.1,vjust=0.5, position=position_dodge(width=0.9),size= 14*0.352777778*relSize)
  q <- q + ThemeShiny(base_size=24*relSize)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("",breaks=pretty_breaks())
  q <- q + coord_flip()
  q <- q + scale_colour_brewer(palette=palette)
  q <- q + scale_fill_brewer(palette=palette,guide=guide_legend(reverse=TRUE,ncol=2,byrow=T))
  q <- q + labs(title=title)
  q <- q + theme(legend.position="bottom")
  return(q)
}

Figure5 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  switch <- c("Ukjent"="Unknown",
              "Under ett år i Norge"="<1 year in Norway",
              "Ett til fire år i Norge"="1-4 years in Norway",
              "5 år eller mer i Norge"="5+ years in Norway",
              "Totalt utenlandsfødte m kjent oppholdstid"="Foreign born with known duration")
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd <- data[isForeignBorn==1]
  RAWmisc::RecodeDT(pd,c("Ubesvart"="Ukjent"),"cPernorNB")
  pd <- pd[,.(isTB=sum(isActive)),by=.(cPernorNB,cyear)]
  pdx <- pd[,.(isTB=sum(isTB)),by=cyear]
  pdx[,cPernorNB:="Totalt utenlandsfødte m kjent oppholdstid"]
  setcolorder(pdx,names(pd))
  pd <- rbind(pd,pdx)
  pd <- pd[cyear>max(cyear)-10 & cPernorNB!="Totalt utenlandsfødte m kjent oppholdstid"]
  
  if(language=="EN") RAWmisc::RecodeDT(pd,switch,"cPernorNB")
  pd[,cPernorNB:=factor(cPernorNB,levels=legendOptions)]
  
  pd[,prettyName:=cPernorNB]
  setorder(pd,-cyear,-prettyName)
  
  pd[,denom:=sum(isTB),by=cyear]
  pd[,perc:=100*isTB/denom]
  pd[,textPos := cumsum(perc)-perc/2,by=cyear]
  pd[,label:=paste0("N=",isTB,"\n",round(perc),"%")]
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Prosent"
    if(relSize<2.5){
      title <- sprintf("Oppholdstid i Norge før diagnose, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Oppholdstid i Norge før diagnose,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Percent"
    if(relSize<2.5){
      title <- sprintf("Residence duration in Norway before diagnosis, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Residence duration in Norway before diagnosis,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  q <- ggplot(pd,aes(x=cyear,y=perc,fill=prettyName))
  q <- q + geom_bar(stat="identity",alpha=0.7)
  q <- q + geom_text(aes(label=label,y=textPos),hjust=0.5,vjust=0.5, size= 14*0.352777778*relSize)
  q <- q + ThemeShiny(base_size=24*relSize)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + scale_y_continuous(titleY,lim=c(0,100),breaks=pretty_breaks())
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + scale_fill_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + theme(legend.position="bottom")
  q <- q + labs(title=title)
  
  return(q)
}


Figure6 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  switch <- c("Afrika"="Africa",
              "Andre europeiske land"="Other European",
              "Asia"="Asia",
              "Tidligere Sovjet"="Previously Soviet",
              "Totalt"="Total")
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[,
              .(isMDR=sum(isMDR,na.rm=T)),
              by=.(cBirthPlaceSovietNB,cyear)]
  pd2 <- data[,
              .(isMDR=sum(isMDR,na.rm=T)),
              by=.(cyear)]
  pd2[,cBirthPlaceSovietNB:="Totalt"]
  setcolorder(pd2,names(pd1))
  pd <- na.omit(rbind(pd1,pd2))
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cBirthPlaceSovietNB),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cBirthPlaceSovietNB"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cBirthPlaceSovietNB"),all.x=TRUE)
  pd[is.na(isMDR), isMDR:=0]
  
  pd <- pd[cyear>max(cyear)-15]
  pd[,totalCases:=sum(isMDR),by=cBirthPlaceSovietNB]
  pd <- pd[totalCases>0]
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Multiresistente tuberkulosetilfeller"
    if(relSize<2.5){
      title <- sprintf("Antall multiresistente tuberkulosetilfeller etter fødested, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Antall multiresistente tuberkulosetilfeller etter fødested,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Multidrug-resistent tuberculosis cases"
    if(relSize<2.5){
      title <- sprintf("Multidrug-resistent tuberculosis cases by birthplace, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Multidrug-resistent tuberculosis cases by birthplace,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  if(type=="a") pd <- pd[cBirthPlaceSovietNB!="Totalt"]
  if(type=="b" & relSize>=2.5) pd[,cBirthPlaceSovietNB:=LineBreak(cBirthPlaceSovietNB, len=15)]

  if(language=="EN") RAWmisc::RecodeDT(pd,switch,"cBirthPlaceSovietNB")
  pd[,prettyName:=cBirthPlaceSovietNB]
  
  if(type=="a"){
    q <- ggplot(pd,aes(x=cyear,y=isMDR,fill=prettyName))
    q <- q + geom_bar(stat="identity",alpha=0.7)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
    q <- q + scale_fill_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
    q <- q + theme(legend.position="bottom")
  } else if(type=="b"){
    q <- ggplot(pd,aes(x=cyear,y=isMDR))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    if(relSize<2.5){
      q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear)),label=lastTwoDigits,minor_breaks=NULL)
    } else {
      q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear),2),label=lastTwoDigits,minor_breaks=NULL)
    }
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,lim=c(0,max(pd$isMDR)),breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + facet_wrap(~prettyName,scales="free")
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
  } 
  
  return(q)
}

Figure7 <- function(data, relSize=1, moreSpace=FALSE, type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  if(relSize<2.5){
    switch <- c("Norskfødte på forebyggende behandling"="Norw. born on preventative treatment",
                "Utenlandsfødte på forebyggende behandling"="Foreign born on preventative treatment",
                "Forebyggende behandling totalt"="Total preventative treatment",
                "Tuberkulosetilfeller"="Tuberculosis cases")
  } else {
    switch <- c("Norskfødte på forebyggende behandling"="Norw. born on preventative treatment",
                "Utenlandsfødte på forebyggende behandling"="Foreign born on preventative treatment",
                "Forebyggende totalt"="Total preventative",
                "Tuberkulosetilfeller"="Tuberculosis cases")
  }
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[,
              .(val=sum(isLatentTB,na.rm=T)),
              by=.(cyear,isForeignBorn)]
  pd2 <- data[,
              .(val=sum(isLatentTB,na.rm=T)),
              by=.(cyear)]
  pd2[,isForeignBorn:=2]
  setcolorder(pd2,names(pd1))
  
  pd3 <- data[,
              .(val=sum(isActive,na.rm=T)),
              by=.(cyear)]
  pd3[,isForeignBorn:=3]
  setcolorder(pd3,names(pd1))
  
  pd <- rbind(pd1, pd2, pd3)
  pd[,prettyName:=factor(isForeignBorn,levels=c("0","1","2","3"))]
  
  pd <- na.omit(pd[cyear > max(cyear)-15])
  
  graphRange <- diff(range(pd$cyear))
  nudgeX <- graphRange*0.15
  if(relSize<2.5){
    extensionX <- graphRange*0.225
  } else {
    extensionX <- graphRange*0.275
  }
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Antall"
    if(relSize<2.5){
      title <- sprintf("Forebyggende behandling av latent tuberkulose fordelt etter opprinnelse,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Forebyggende behandling av latent tuberkulose\nfordelt etter opprinnelse, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Number"
    if(relSize<2.5){
      title <- sprintf("Preventative treatment of latent tuberculosis by origin, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Preventative treatment of latent tuberculosis\nby origin, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  if(type=="a"){
    levels(pd$prettyName) <- legendOptions
    
    q <- ggplot(pd,aes(x=cyear,y=val,colour=prettyName))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + theme(legend.position="bottom")
  } else {
    if(relSize<=2.5){
      levels(pd$prettyName) <- LineBreak(legendOptions,len=20)
    } else {
      levels(pd$prettyName) <- LineBreak(legendOptions,len=15)
    }
    
    q <- ggplot(pd,aes(x=cyear,y=val,colour=prettyName,labelSize=1.75*relSize,nudgeX=0.4*relSize,bottomY=0))
    q <- q + geom_rect(xmin=max(pd$cyear),xmax=Inf,ymin=-Inf,ymax=Inf,fill="white",colour=NA)
    q <- q + geom_line(lwd=2*relSize)
    q <- q +
      directlabels::geom_dl(data=pd,mapping=aes(label = prettyName), method=labelMethodRight)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous(titleX,breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0,x=max(pd$cyear)+extensionX)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
  }

  return(q)
}



Figure8 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  switch <- c(
    "Totalt"="Total",
    "Utenlandsfødte"="Foreign born",
    "Norskfødt med to norskfødte foreldre"="Nor. born: 2 Nor. born parents",
    "Norskfødt med minst en utenlandsfødt forelder"="Nor. born: >=1 foreign born parents"
  )
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[,
              .(isLatentTB=sum(isLatentTB,na.rm=T)),
              by=.(cAlgr,cNorwegianStatusNB,cyear)]
  pd2 <- data[,
              .(isLatentTB=sum(isLatentTB,na.rm=T)),
              by=.(cAlgr,cyear)]
  pd2[,cNorwegianStatusNB:="Totalt"]
  setcolorder(pd2,names(pd1))
  pd <- na.omit(rbind(pd1,pd2))
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cNorwegianStatusNB),unique(pd$cAlgr),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cNorwegianStatusNB","cAlgr"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cNorwegianStatusNB","cAlgr"),all.x=TRUE)
  pd[is.na(isLatentTB), isLatentTB:=0]
  
  pd <- pd[cyear==max(cyear)]
  if(language=="EN") RAWmisc::RecodeDT(pd,switch,"cNorwegianStatusNB")
  
  xLabs <- sort(unique(pd$cAlgr))
  xVals <- 1:length(xLabs)
  pd[,x:=as.numeric(factor(cAlgr,levels=xLabs))]
  
  graphRange <- diff(range(pd$x))
  nudgeX <- graphRange*0.15
  if(relSize<2.5){
    extensionX <- graphRange*0.275
  } else {
    extensionX <- graphRange*0.335
  }
  
  if(language=="NB"){
    titleX <- "Alder"
    titleY <- "Antall med forebyggende behandling"
    if(relSize<2.5){
      title <- sprintf("Forebyggende behandling etter aldersgruppe, for norskfødte og utenlandsfødte,\nMSIS %s",min(pd$cyear))
    } else {
      title <- sprintf("Forebyggende behandling etter aldersgruppe,\nfor norskfødte og utenlandsfødte, MSIS %s",min(pd$cyear))
    }
  } else {
    titleX <- "Age"
    titleY <- "Number with preventative treatment"
    if(relSize<2.5){
      title <- sprintf("Preventative treatment by age, for Norwegian and foreign born,\nMSIS %s ",min(pd$cyear))
    } else {
      title <- sprintf("Preventative treatment by age,\nNorwegian and foreign born, MSIS %s ",min(pd$cyear))
    }
  }
  
  if(type=="a"){
    pd[,prettyName:=cNorwegianStatusNB]
    q <- ggplot(pd,aes(x=x,y=isLatentTB,colour=prettyName,labelSize=1.75*relSize,nudgeX=-0.4*relSize,bottomY=0))
    q <- q + geom_line(lwd=2*relSize)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_x_continuous("",breaks=xVals,labels=xLabs, minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0)
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + theme(legend.position="bottom")

    
  } else {
    pd[,prettyName:=LineBreak(cNorwegianStatusNB,len=25)]
    q <- ggplot(pd,aes(x=x,y=isLatentTB,colour=prettyName,labelSize=1.75*relSize,nudgeX=-0.4*relSize,bottomY=0))
    q <- q + geom_rect(xmin=-Inf,xmax=1,ymin=-Inf,ymax=Inf,fill="white",colour=NA)
    q <- q + geom_line(lwd=2*relSize)
    q <- q +
      directlabels::geom_dl(data=pd,mapping=aes(label = prettyName), method=labelMethodLeft)
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(title=title)
    q <- q + expand_limits(y=0,x=min(pd$x)-extensionX)
    q <- q + scale_colour_brewer(palette=palette)
    q <- q + scale_fill_brewer(palette=palette)
    q <- q + guides(fill="none")
    q <- q + guides(colour="none")
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    if(relSize<2.5){
      q <- q + scale_x_continuous(titleX,breaks=xVals,labels=xLabs, minor_breaks=NULL)
    } else {
      q <- q + scale_x_continuous(titleX,breaks=xVals,labels=xLabs, minor_breaks=NULL,lim=c(-3,10))
    }
  }
  q
  return(q)
}

Figure9 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  switch <- c(
    "Ubesvart/ukjent"="Unanswered/unknown",
    "Annen indikasjon"="Other indication",
    "Symptomer eller tegn"="Symptoms or signs",
    "Smitteoppsporing"="Contact tracing",
    "Screening, arbeid"="Screening, work",
    "Screening, immunsvekkelse"="Screening, immunocompromised",
    "Screening, innvandring"="Screening, immigrant"
  )
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd <- copy(data)
  pd[,newIndik:=as.character(NA)]
  pd[cIndikNB %in% c(
    "Arbeid med pasienter eller barn"
  ), newIndik:="Screening, arbeid"]
  
  pd[cIndikNB %in% c(
    "Immunsvekkende tilstand/behandling"
  ), newIndik:="Screening, immunsvekkelse"]
  
  pd[cIndikNB %in% c(
    "Rutineundersøkelse av innvandrer"
  ), newIndik:="Screening, innvandring"]
  
  pd[cIndikNB %in% c(
    "Smitteoppsporing (miljøundersøkelse)"
  ), newIndik:="Smitteoppsporing"]
  
  pd[cIndikNB %in% c(
    "Symptomer eller tegn"
  ), newIndik:="Symptomer eller tegn"]
  
  pd[cIndikNB %in% c(
    "Ubesvart",
    "Ukjent"
  ), newIndik:="Ubesvart/ukjent"]
  
  pd[is.na(newIndik) & !is.na(cIndikNB), newIndik:="Annen indikasjon"]
  
  if(language=="EN") RAWmisc::RecodeDT(pd,switch,"newIndik")
  pd[,newIndik := factor(newIndik,levels=legendOptions)]
  
  pd <- pd[cyear>max(cyear)-10,.(isTB=sum(isActive)),by=.(newIndik,cyear)]
  setorder(pd,-cyear,newIndik)
  setcolorder(pd,c("cyear","newIndik","isTB"))
  
  pd[,prettyName:=newIndik]
  setorder(pd,-cyear,-newIndik)
  
  pd[,denom:=sum(isTB),by=cyear]
  pd[,perc:=100*isTB/denom]
  pd[,textPos := cumsum(perc)-perc/2,by=cyear]
  pd[,label:=paste0("N=",isTB,"\n",round(perc),"%")]
  pd[perc<10,label:=paste0(round(perc),"%")]
  pd[perc<=2,label:=""]
  
  if(language=="NB"){
    titleX <- "År"
    titleY <- "Prosent"
    if(relSize<2.5){
      title <- sprintf("Indikasjon for tuberkuloseundersøkelse, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Indikasjon for tuberkuloseundersøkelse,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  } else {
    titleX <- "Year"
    titleY <- "Percent"
    if(relSize<2.5){
      title <- sprintf("Indication for tubuerculosis examination, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Indication for tubuerculosis examination, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  q <- ggplot(pd,aes(x=cyear,y=perc,fill=prettyName))
  q <- q + geom_bar(stat="identity",alpha=0.7)
  q <- q + geom_text(aes(label=label,y=textPos),hjust=0.5,vjust=0.5, size= 14*0.352777778*relSize)
  q <- q + ThemeShiny(base_size=24*relSize)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + scale_y_continuous(titleY,lim=c(0,100),breaks=pretty_breaks())
  q <- q + labs(title=title)
  q <- q + expand_limits(y=0)
  q <- q + scale_fill_manual(NULL,values=c(
      "#1a9850",
      "#cab2d6",
      "#abd9e9",
      "#fee090",
      "#fdae61",
      "#f46d43",
      "#d73027"
    ),guide=guide_legend(ncol=3,byrow=T))
  q <- q + theme(legend.position="bottom")
  #q
  return(q)
}



Figure10 <- function(data, relSize=1, moreSpace=FALSE,type="a", language="NB"){
  if(is.null(relSize)) relSize <- 1
  
  if(relSize<2.5){
    switchNB <- c(
      "isActive"="Meldte TB",
      "isCulturePos"="Dyrkningspos",
      "isRRes"="Rifampicin",
      "isHRes"="Isoniazid",
      "isPRes"="Pyrazinamide",
      "isERes"="Ethambutol",
      "isSRes"="Streptomycin",
      "isMDR"="MDR-TB"
    )
    
    switchEN <- c(
      "isActive"="Registered TB",
      "isCulturePos"="Culture positive",
      "isRRes"="Rifampicin",
      "isHRes"="Isoniazid",
      "isPRes"="Pyrazinamide",
      "isERes"="Ethambutol",
      "isSRes"="Streptomycin",
      "isMDR"="MDR-TB"
    )
  } else {
    switchNB <- c(
      "isActive"="Meldte TB",
      "isCulturePos"="Dyrkningspos",
      "isRRes"="RIF",
      "isHRes"="INH",
      "isPRes"="PZA",
      "isERes"="EMB",
      "isSRes"="STM",
      "isMDR"="MDR-TB"
    )
    
    switchEN <- c(
      "isActive"="Registered TB",
      "isCulturePos"="Culture positive",
      "isRRes"="RIF",
      "isHRes"="INH",
      "isPRes"="PZA",
      "isERes"="EMB",
      "isSRes"="STM",
      "isMDR"="MDR-TB"
    )
  }
  
  switch <- list(
    "NB"=switchNB,
    "EN"=switchEN
  )[[language]]
  
  pd <- copy(data)
  pd <- pd[isActive==1 & cyear>max(cyear)-10,.(
    isActive=sum(isActive,na.rm=T),
    isCulturePos=sum(cDyrkResAllOrganNB=="Positivt",na.rm=T),
    isRRes=sum(isRRes,na.rm=T),
    isHRes=sum(isHRes,na.rm=T),
    isPRes=sum(isPRes,na.rm=T),
    isERes=sum(isERes,na.rm=T),
    isSRes=sum(isSRes,na.rm=T),
    isMDR=sum(isMDR,na.rm=T)
  ),by=cyear]
  pd[,isRRes:=isRRes/isCulturePos*100]
  pd[,isHRes:=isHRes/isCulturePos*100]
  pd[,isPRes:=isPRes/isCulturePos*100]
  pd[,isERes:=isERes/isCulturePos*100]
  pd[,isSRes:=isSRes/isCulturePos*100]
  pd[,isMDR:=isMDR/isCulturePos*100]
  
  # manual cleaning for not yet reported stuff
  pd[cyear>=2016,isSRes:=NA]
  
  pd <- melt.data.table(pd,id="cyear")
  
  if(type=="a"){
    pd <- pd[!variable%in%c("isActive","isCulturePos")]
    
    if(language=="NB"){
      if(relSize<2.5){
        title <- sprintf("Prosentvis resistens av totalt antall dyrkningspositive meldte tuberkulosetilfeller,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
      } else {
        title <- sprintf("Prosentvis resistens av totalt antall dyrkningspositive\nmeldte tuberkulosetilfeller, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
      }
    } else {
      if(relSize<2.5){
        title <- sprintf("Percent resistance in total culture positive registered TB cases,\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
      } else {
        title <- sprintf("Percent resistance in total culture positive\nregistered TB cases, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
      }
    }
    
  } else {
    if(language=="NB"){
      if(relSize<2.5){
        pd[,topGraph:=sprintf(" \nProsentvis resistens av totalt antall dyrkningspositive meldte tuberkulosetilfeller, MSIS %s - %s",min(cyear),max(cyear))]
        pd[variable%in%c("isActive","isCulturePos"),topGraph:=sprintf("\nTuberkulosetilfeller, MSIS %s - %s",min(cyear),max(cyear))]
      } else {
        pd[,topGraph:=" \nProsentvis resistens av dyrkningspositive meldte tuberkulosetilfeller"]
        pd[variable%in%c("isActive","isCulturePos"),topGraph:=sprintf("\nTuberkulosetilfeller, MSIS %s - %s",min(cyear),max(cyear))]
      }
    } else {
      if(relSize<2.5){
        pd[,topGraph:=sprintf(" \nPercent resistance in total culture positive registered TB cases, MSIS %s - %s",min(cyear),max(cyear))]
        pd[variable%in%c("isActive","isCulturePos"),topGraph:=sprintf("\nTuberculosis cases, MSIS %s - %s",min(cyear),max(cyear))]
      } else {
        pd[,topGraph:=" \nPercent resistance in total culture positive registered TB cases"]
        pd[variable%in%c("isActive","isCulturePos"),topGraph:=sprintf("\nTuberculosis cases, MSIS %s - %s",min(cyear),max(cyear))]
      }
    }
  }
  
  RAWmisc::RecodeDT(pd,switch,"variable")
  
  if(type=="a"){
    q <- ggplot(pd,aes(x=cyear,y=value,colour=variable,group=variable))
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + geom_line(lwd=2*relSize)
    q <- q + expand_limits(y=c(0,25))
    q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    q <- q + scale_y_continuous("Prosent")
    q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
    q <- q + labs(title=title)
    q <- q + theme(legend.position="bottom")
  } else {
    q <- ggplot(pd,aes(x=variable,y=value,fill=factor(cyear),group=factor(cyear)))
    q <- q + geom_bar(stat="identity",pos="dodge",colour="black")
    q <- q + facet_wrap(~topGraph,ncol=1,scales="free")
    q <- q + expand_limits(y=100)
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("")
    q <- q + scale_fill_brewer(NULL,palette=palette,guide=guide_legend(ncol=10,byrow=T))
    q <- q + ThemeShiny(base_size=24*relSize)
    q <- q + theme(legend.position="bottom")
  }
  
  return(q)
}

