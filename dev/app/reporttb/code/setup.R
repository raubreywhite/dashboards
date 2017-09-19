xxThemeShiny <<- function(base_size = 12, base_family = "") {
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white",  colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
                                                                                                          face = "plain", color = "black", size = base_size, hjust = 0.5, 
                                                                                                          vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), 
                                                                                                          debug = FALSE), axis.text = element_text(size = rel(0.8), 
                                                                                                                                                   colour = "black"), strip.text = element_text(size = rel(0.8), 
                                                                                                                                                                                                colour = "black"), axis.line.x = element_line(size = base_size/20), 
    axis.line.y = element_line(size = base_size/20), axis.text.x = element_text(vjust = 1, 
                                                                                margin = margin(5, 5, 10, 5, "pt")), axis.text.y = element_text(hjust = 1, 
                                                                                                                                                margin = margin(5, 5, 10, 5, "pt")), axis.ticks = element_line(), 
    axis.title = element_text(colour = "black"), axis.title.x = element_text(vjust = 1), 
    axis.title.y = element_text(angle = 90, vjust = 1), axis.ticks.length = unit(0.3, 
                                                                                 "lines"), legend.background = element_rect(colour = NA), 
    legend.margin = unit(0.01*base_size, "lines"), legend.key = element_rect(fill = "white", 
                                                                             colour = "black"), legend.key.size = unit(0.1 * base_size, 
                                                                                                                       "lines"), legend.key.height = NULL, legend.key.width = NULL, 
    legend.text = element_text(size = rel(0.8), colour = "black"), 
    legend.text.align = NULL, legend.title = element_text(size = rel(0.8), 
                                                          face = "bold", hjust = 0, colour = "white"), legend.title.align = NULL, 
    legend.position = "bottom", legend.direction = "horizontal", 
    legend.justification = "center", legend.box = NULL, panel.background = element_rect(fill = NA, 
                                                                                        colour = NA), panel.border = element_rect(fill = NA, 
                                                                                                                                  colour = NA), panel.grid.major = element_line(colour = "black", 
                                                                                                                                                                                size = rel(0.8), linetype = 3), panel.grid.minor = element_line(colour = "black", 
                                                                                                                                                                                                                                                size = rel(0.8), linetype = 3), panel.margin = unit(0.25, 
                                                                                                                                                                                                                                                                                                    "lines"), strip.background = element_rect(fill = "white", 
                                                                                                                                                                                                                                                                                                                                              colour = "white", size = 3), strip.text.x = element_text(), 
    strip.text.y = element_text(angle = -90), plot.background = element_rect(colour = NA, 
                                                                             fill = NA), plot.title = element_text(size = rel(1.2)), 
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"), complete = TRUE)
}

ThemeShiny <<- function(base_size = 24, base_family = "") {
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      color = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                      margin = margin(), debug = FALSE),
    axis.text =          element_text(size = rel(1), colour = "black", margin=margin(unit(c(2, 2, 2, 2), "lines"))),
    strip.text =         element_text(size = rel(1), colour = "black"),
    
    axis.line =        element_line(size=base_size/20, color="black"),
    axis.line.x =        element_line(size=base_size/20, color="black"),
    axis.line.y =        element_line(size=base_size/20, color="black"),
    axis.text.x =        element_text(vjust = 1, margin=margin(0.01*24*sqrt(base_size/24),0,0.02*24*sqrt(base_size/24),0,"lines")),
    axis.text.y =        element_text(hjust = 1, margin=margin(0,0.01*24*sqrt(base_size/24),0,0.02*24*sqrt(base_size/24),"lines")),
    axis.ticks =         element_line(),
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(vjust = 0, margin=margin(5,5,10,5,"pt")),
    axis.title.y =       element_text(angle = 90, vjust=0, margin=margin(5,5,10,5,"pt")),
    axis.ticks.length =  unit(0.03*base_size, "lines"),
    
    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "white", colour = "black"),
    legend.key.size =    unit(0.1*base_size, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8), colour = "black"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0, colour = "white"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = NA, colour = NA),
    panel.border =       element_rect(fill = NA, colour = NA),
    panel.grid.major =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.grid.minor =   element_line(colour = "black", size = rel(0.8), linetype=3),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "white", colour = "white", size=3),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),
    
    plot.background =    element_rect(colour = NA, fill = NA),
    plot.title =         element_text(size = rel(1.2), vjust=1, margin=margin(0,0,0.03*24*sqrt(base_size/24),0,"lines")),
    plot.margin =        unit(c(2, 2, 2, 2), "lines"),
    
    panel.margin =        unit(c(2, 2, 2, 2), "lines"),
    
    complete = TRUE
  )
}

LineBreak <<- function(x,end=FALSE,len=20){
  x <- gsub("/", "/ ", x)
  x <- gsub(sprintf("(.{1,%d})(\\s|$)",len), "\\1\n", x)
  x <- gsub("/ ", "/", x)
  if(!end) x <- gsub("\\n$","",x)
  return(x)
}

labelMethodLeft <- list()
labelMethodLeft[[1]] <- "first.points"
labelMethodLeft[[2]] <- "setCEX"
labelMethodLeft[[3]] <- "setNudgeX"
labelMethodLeft[[4]] <- directlabels::last.qp[[3]]
labelMethodLeft[[5]] <- "setBottomY"
labelMethodLeft[[6]] <- "setBumpUp"


labelMethodRight <- list()
labelMethodRight[[1]] <- "last.points"
labelMethodRight[[2]] <- "setCEX"
labelMethodRight[[3]] <- "setNudgeX"
labelMethodRight[[4]] <- directlabels::last.qp[[3]]
labelMethodRight[[5]] <- "setBottomY"
labelMethodRight[[6]] <- "setBumpUp"


setCEX <- function (d, ...) 
{
  d <- calc.boxes(d)
  l <- xlimits()
  positive.part <- function(x) ifelse(x > 0, x, 0)
  right <- positive.part(d$right - l[2])
  left <- positive.part(l[1] - d$left)
  w <- d$right - d$left
  if (is.null(d$cex)) {
    d$cex <- 1
  }
  d$cex <- as.numeric(d$labelSize)
  print(d)
  calc.boxes(d)
}

setNudgeX <- function (d, ...) 
{
  d <- calc.boxes(d)
  l <- xlimits()
  positive.part <- function(x) ifelse(x > 0, x, 0)
  right <- positive.part(d$right - l[2])
  left <- positive.part(l[1] - d$left)
  w <- d$right - d$left
  d$x <- d$x + as.numeric(d$nudgeX)
  print(d)
  calc.boxes(d)
}

setBottomY <- function (d, ...) 
{
  d <- internalCalcBoxes(d,multipy=FALSE)
  print(d)
  d$belowBottom <- d$bottomY - d$bottom
  d$y[d$belowBottom>0] <- d$y[d$belowBottom>0] + d$belowBottom[d$belowBottom>0]
  calc.boxes(d)
}

internalCalcBoxes <- function
### Calculate boxes around labels, for collision detection.
(d,
 debug=FALSE,
 multiply=TRUE,
 ...
){
  vp <- grid::current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      grid::pushViewport(vp)
      if(debug)grid::grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"cm")
      grid::popViewport()
      w
    }))
  }
  ## abs since we have a weird bug with ggplot2 sometimes
  d$w <- abs(convert("Width"))
  d$h <- abs(convert("Height"))
  if(multiply) d$h <- d$h*1.25
  directlabels::calc.borders(d)
}

setBumpUp <- function (d, ...) 
{
  if (nrow(d) == 1) 
    return(d)
  d <- internalCalcBoxes(d)[order(d$y), ]
  "%between%" <- function(v, lims) lims[1] < v & v < lims[2]
  obox <- function(x, y) {
    tocheck <- with(x, c(left, (right - left)/2 + left, right))
    tocheck %between% with(y, c(left, right))
  }
  for (i in 2:nrow(d)) {
    dif <- d$bottom[i] - d$top[i - 1]
    overlap <- c(obox(d[i, ], d[i - 1, ]), obox(d[i - 1, 
                                                  ], d[i, ]))
    if (dif < 0 && any(overlap)) {
      d$bottom[i] <- d$bottom[i] - dif
      d$top[i] <- d$top[i] - dif
      d$y[i] <- d$y[i] - dif
    }
  }
  d
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

lastTwoDigits <- function(x) {
  paste0("'",substrRight(as.character(x),2))
}


