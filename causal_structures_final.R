### R script to explore causal graph structure of ASTHMA prevalence

library(raster)
library(rgdal)
library(rgeos)
library(pcalg)
library(MASS)
library(devtools)
#install_github("ericstrobl/RCIT")
library(RCIT)



perth_data <- read.csv("asthma_maxent_Perth.csv")
perth_extradata <- read.csv("extracovs_Perth.csv")
brisbane_data <- read.csv("asthma_maxent_Brisbane.csv")
brisbane_extradata <- read.csv("extracovs_Brisbane.csv")
melbourne_data <- read.csv("asthma_maxent_Melbourne.csv")
melbourne_extradata <- read.csv("extracovs_Melbourne.csv")
sydney_data <- read.csv("asthma_maxent_Sydney.csv")
sydney_extradata <- read.csv("extracovs_Sydney.csv")



perth_data <- cbind(perth_data[,-1],perth_extradata[match(perth_extradata$SA1_CODE21,perth_data$SA1_CODE21),-(1:2)])
brisbane_data <- cbind(brisbane_data[,-1],brisbane_extradata[match(brisbane_extradata$SA1_CODE21,brisbane_data$SA1_CODE21),-(1:2)])
melbourne_data <- cbind(melbourne_data[,-1],melbourne_extradata[match(melbourne_extradata$SA1_CODE21,melbourne_data$SA1_CODE21),-(1:2)])
sydney_data <- cbind(sydney_data[,-1],sydney_extradata[match(sydney_extradata$SA1_CODE21,sydney_data$SA1_CODE21),-(1:2)])



### Initial testing on a city-by-city basis



plotAllDags <- function(res) {
  require(graph)
  p <- sqrt(ncol(res$dags))
  nDags <- ceiling(sqrt(nrow(res$dags)))
  par(mfrow = c(nDags, nDags))
  for (i in 1:nrow(res$dags)) {
    tmp <- matrix(res$dags[i,],p,p)
    colnames(tmp) <- rownames(tmp) <- res$nodeNms
    plot(as(tmp, "graphNEL"))
  }
}



# Perth
space_perth <- as.matrix(perth_data[,c(4:5)])
perth_data <- perth_data[,c(6,2,3,7:16)]
xnames <- colnames(perth_data)
space_perth <- space_perth[perth_data$EST_POP_5_9>10,]
perth_data <- perth_data[perth_data$EST_POP_5_9>10,]



space_sydney <- as.matrix(sydney_data[,c(4:5)])
sydney_data <- sydney_data[,c(6,2,3,7:16)]
xnames <- colnames(sydney_data)
space_sydney <- space_sydney[sydney_data$EST_POP_5_9>10,]
sydney_data <- sydney_data[sydney_data$EST_POP_5_9>10,]



space_melbourne <- as.matrix(melbourne_data[,c(4:5)])
melbourne_data <- melbourne_data[,c(6,2,3,7:16)]
xnames <- colnames(melbourne_data)
space_melbourne <- space_melbourne[melbourne_data$EST_POP_5_9>10,]
melbourne_data <- melbourne_data[melbourne_data$EST_POP_5_9>10,]



space_brisbane <- as.matrix(brisbane_data[,c(4:5)])
brisbane_data <- brisbane_data[,c(6,2,3,7:16)]
xnames <- colnames(brisbane_data)
space_brisbane <- space_brisbane[brisbane_data$EST_POP_5_9>10,]
brisbane_data <- brisbane_data[brisbane_data$EST_POP_5_9>10,]




suffStat <- list()
for (i in 1:ncol(perth_data)) {suffStat[[i]] <- as.numeric(perth_data[,i])}
suffStat[[length(suffStat)+1]] <- matrix(space_perth,ncol=2)



RCITtest <- function(x,y,S,suffStat) { # prepare RCIT in the format that pcalg expects its tests
  
  if (length(x)==1) {xx <- suffStat[[x]]}
  if (length(y)==1) {yy <- suffStat[[y]]}
  if (length(S)==1) {SS <- suffStat[[S]]}
  if (length(x)>1) {xx <- do.call(cbind,suffStat[x])}
  if (length(y)>1) {yy <- do.call(cbind,suffStat[y])}
  if (length(S)>1) {SS <- do.call(cbind,suffStat[S])}
  
  if (is.null(dim(xx))) {
    if (length(S)==0) {
      RCIT(xx,yy,num_f=50)$p 
    } else {
      RCIT(xx,yy,SS,num_f=50,num_f2=50)$p 
    }
  } else {
    if (length(S)==0) {
      RCIT(yy,xx,num_f=50)$p 
    } else {
      RCIT(yy,xx,SS,num_f=50,num_f2=50)$p 
    }
  }
}



xnames_short <- c("A","Pop","SES","LST","LSTD","EVI","TCB","TCW","ELE","D","H","Ind","Com","space")



cat("Perth Sydney Melbourne Brisbane\n")
for (j in 2:14) {
  suffStat <- list()
  for (i in 1:ncol(perth_data)) {suffStat[[i]] <- as.numeric(perth_data[,i])}
  suffStat[[length(suffStat)+1]] <- matrix(space_perth,ncol=2)
  
  cat(xnames_short[j]," ",RCITtest(1,j,NULL,suffStat))
  
  suffStat <- list()
  for (i in 1:ncol(sydney_data)) {suffStat[[i]] <- as.numeric(sydney_data[,i])}
  suffStat[[length(suffStat)+1]] <- matrix(space_sydney,ncol=2)
  
  cat(" ",RCITtest(1,j,NULL,suffStat))
  
  suffStat <- list()
  for (i in 1:ncol(melbourne_data)) {suffStat[[i]] <- as.numeric(melbourne_data[,i])}
  suffStat[[length(suffStat)+1]] <- matrix(space_melbourne,ncol=2)
  
  cat(" ",RCITtest(1,j,NULL,suffStat))
  
  suffStat <- list()
  for (i in 1:ncol(brisbane_data)) {suffStat[[i]] <- as.numeric(brisbane_data[,i])}
  suffStat[[length(suffStat)+1]] <- matrix(space_brisbane,ncol=2)
  
  cat(" ",RCITtest(1,j,NULL,suffStat),"\n")
}

