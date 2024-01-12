### Model for the count of ASTHMA per SA1 unit over Australia's four largest cities
setwd("/users/joycemo/documents/summer2023/TKI/")

# asthma data from census "whether has asthma" 
filename_SA1_Perth <- "SA1_by_HASTP_Perth_59.csv"
filename_SA2_Perth <- "SA2_by_HASTP_Perth_59.csv"

filename_SA1_Brisbane <- "SA1_by_HASTP_Brisbane_59.csv"
filename_SA2_Brisbane <- "SA2_by_HASTP_Brisbane_59.csv"

filename_SA1_Melbourne <- "SA1_by_HASTP_Melbourne_59.csv"
filename_SA2_Melbourne <- "SA2_by_HASTP_Melbourne_59.csv"

filename_SA1_Sydney <- "SA1_by_HASTP_Sydney_59.csv"
filename_SA2_Sydney <- "SA2_by_HASTP_Sydney_59.csv"

library(rgdal)
library(MASS)
library(Matrix)
library(truncnorm)

### Read in and prepare data


sa1_2021_Perth <- readOGR("sa1_2021_Perth_dist.shp")
sa1_2021_Brisbane <- readOGR("sa1_2021_Brisbane_dist.shp")
sa1_2021_Melbourne <- readOGR("sa1_2021_Melbourne_dist.shp")
sa1_2021_Sydney <- readOGR("sa1_2021_Sydney_dist.shp")

# SA1 level Tablebuilder Outputs
xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI//",filename_SA1_Perth),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Perth),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021_Perth$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM_Perth <- SA1_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Brisbane),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Brisbane),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021_Brisbane$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM_Brisbane <- SA1_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Melbourne),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Melbourne),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021_Melbourne$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM_Melbourne <- SA1_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Sydney),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA1_Sydney),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021_Sydney$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM_Sydney <- SA1_num_dwellings_by_DATUM

# SA2 level Tablebuilder Outputs
xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Perth),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Perth),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021_Perth$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM_Perth <- SA2_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Brisbane),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Brisbane),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021_Brisbane$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM_Brisbane <- SA2_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Melbourne),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Melbourne),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021_Melbourne$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM_Melbourne <- SA2_num_dwellings_by_DATUM

xdata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Sydney),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/users/joycemo/documents/summer2023/TKI/",filename_SA2_Sydney),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021_Sydney$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM_Sydney <- SA2_num_dwellings_by_DATUM

sa1sa2codes_Perth <- match(sa1_2021_Perth$SA2_NAME21,unique(sa1_2021_Perth$SA2_NAME21))
sa1sa2codes_Brisbane <- match(sa1_2021_Brisbane$SA2_NAME21,unique(sa1_2021_Brisbane$SA2_NAME21))
sa1sa2codes_Melbourne <- match(sa1_2021_Melbourne$SA2_NAME21,unique(sa1_2021_Melbourne$SA2_NAME21))
sa1sa2codes_Sydney <- match(sa1_2021_Sydney$SA2_NAME21,unique(sa1_2021_Sydney$SA2_NAME21))

M <- dim(SA1_num_dwellings_by_DATUM_Perth)[2]-2

SA1_num_dwellings_by_DATUM_obs <- rbind(
  SA1_num_dwellings_by_DATUM_Perth[,-c(1,M+2)],
  SA1_num_dwellings_by_DATUM_Brisbane[,-c(1,M+2)],
  SA1_num_dwellings_by_DATUM_Melbourne[,-c(1,M+2)],
  SA1_num_dwellings_by_DATUM_Sydney[,-c(1,M+2)]
)

SA1_num_dwellings_obs <- c(
  SA1_num_dwellings_by_DATUM_Perth[,M+2],
  SA1_num_dwellings_by_DATUM_Brisbane[,M+2],
  SA1_num_dwellings_by_DATUM_Melbourne[,M+2],
  SA1_num_dwellings_by_DATUM_Sydney[,M+2]
)

SA2_num_dwellings_by_DATUM_obs <- rbind(
  SA2_num_dwellings_by_DATUM_Perth[,-c(1,M+2)],
  SA2_num_dwellings_by_DATUM_Brisbane[,-c(1,M+2)],
  SA2_num_dwellings_by_DATUM_Melbourne[,-c(1,M+2)],
  SA2_num_dwellings_by_DATUM_Sydney[,-c(1,M+2)]
)

SA2_num_dwellings_obs <- c(
  SA2_num_dwellings_by_DATUM_Perth[,M+2],
  SA2_num_dwellings_by_DATUM_Brisbane[,M+2],
  SA2_num_dwellings_by_DATUM_Melbourne[,M+2],
  SA2_num_dwellings_by_DATUM_Sydney[,M+2]
)


sa1sa2codes <- c(
  sa1sa2codes_Perth,
  sa1sa2codes_Brisbane+max(sa1sa2codes_Perth),
  sa1sa2codes_Melbourne+max(sa1sa2codes_Perth)+max(sa1sa2codes_Brisbane),
  sa1sa2codes_Sydney+max(sa1sa2codes_Perth)+max(sa1sa2codes_Brisbane)+max(sa1sa2codes_Melbourne)
)

### Covariates

perth_data <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/asthma_maxent_Perth.csv")
perth_extradata <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/extracovs_Perth.csv")
brisbane_data <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/asthma_maxent_Brisbane.csv")
brisbane_extradata <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/extracovs_Brisbane.csv")
melbourne_data <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/asthma_maxent_Melbourne.csv")
melbourne_extradata <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/extracovs_Melbourne.csv")
sydney_data <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/asthma_maxent_Sydney.csv")
sydney_extradata <- read.csv("/users/joycemo/documents/summer2023/TKI/asthma_data/extracovs_Sydney.csv")

###/// new code to combien no2 data with the 4 city covariates ("extradata" csv files)
perth_no2 <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/no2_perth_2021_data.csv")
brisbane_no2 <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/no2_brisbane_2021_data.csv")
melbourne_no2 <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/no2_melbourne_2021_data.csv")
sydney_no2 <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/no2_sydney_2021_data.csv")

# Add the mean NO2 column to the respective city data frames
perth_extradata$NO2 <- perth_no2[, 13]
brisbane_extradata$NO2 <- brisbane_no2[, 13]
melbourne_extradata$NO2 <- melbourne_no2[, 13]
sydney_extradata$NO2 <- sydney_no2[, 13]

perth_extradata <- cbind(perth_extradata, perth_no2)
brisbane_extradata <- cbind(brisbane_extradata, brisbane_no2)
melbourne_extradata <- cbind(melbourne_extradata, melbourne_no2)
sydney_extradata <- cbind(sydney_extradata, sydney_no2)

# Add the mean CO column to the respective city data frames 
## add CO means to the extradata dataframes 
perth_co <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/co_perth_2021_data.csv")
brisbane_co <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/co_brisbane_2021_data.csv")
melbourne_co <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/co_melbourne_2021_data.csv")
sydney_co <- read.csv("/users/joycemo/documents/summer2023/TKI/4_cities_no2/co_sydney_2021_data.csv")

# Add the mean CO column to the respective city data frames
perth_extradata$CO <- perth_co[, 13]
brisbane_extradata$CO <- brisbane_co[, 13]
melbourne_extradata$CO <- melbourne_co[, 13]
sydney_extradata$CO <- sydney_co[,13]

perth_extradata <- cbind(perth_extradata, perth_co)
brisbane_extradata <- cbind(brisbane_extradata, brisbane_co)
melbourne_extradata <- cbind(melbourne_extradata, melbourne_co)
sydney_extradata <- cbind(sydney_extradata, sydney_co)

perth_data <- cbind(perth_data[,-1],perth_extradata[match(perth_extradata$SA1_CODE21,perth_data$SA1_CODE21),-(1:2)])
brisbane_data <- cbind(brisbane_data[,-1],brisbane_extradata[match(brisbane_extradata$SA1_CODE21,brisbane_data$SA1_CODE21),-(1:2)])
melbourne_data <- cbind(melbourne_data[,-1],melbourne_extradata[match(melbourne_extradata$SA1_CODE21,melbourne_data$SA1_CODE21),-(1:2)])
sydney_data <- cbind(sydney_data[,-1],sydney_extradata[match(sydney_extradata$SA1_CODE21,sydney_data$SA1_CODE21),-(1:2)])

joint_covariates <- rbind(
  as.matrix(perth_data),
  as.matrix(brisbane_data),
  as.matrix(melbourne_data),
  as.matrix(sydney_data)
)
joint_covariates <- joint_covariates[,-c(1,2,4,5,6)]

K <- dim(joint_covariates)[2]
for (j in 1:K) {
  joint_covariates[,j] <- (joint_covariates[,j]-mean(joint_covariates[,j]))/sd(joint_covariates[,j])
}
joint_covariates[joint_covariates>5] <- 5
joint_covariates[joint_covariates< -5] <- -5

### Fit Regression-Based Proposal Model

true_SA1_num_dwellings_by_DATUM_current <- SA1_num_dwellings_by_DATUM_obs+1 # this ensures a valid guess for every cell but means the starting point is high relative to the optimal solution

true_SA1_num_dwellings_current <- rowSums(true_SA1_num_dwellings_by_DATUM_current)
true_SA2_num_dwellings_by_DATUM_current <- as.matrix(aggregate(true_SA1_num_dwellings_by_DATUM_current,list(sa1sa2codes),sum)[,-1])
true_SA2_num_dwellings_current <- rowSums(true_SA2_num_dwellings_by_DATUM_current)

error_sd <- 2.0

logdiffexp <- function(y,x) {
  # x > y
  x+log(1-exp(y-x))
}
logsumexp <- function(y,x) {
  if (length(x)==1) {
  max(c(x,y))+log(1+exp(min(c(x,y))-max(c(x,y))))} else {
    mmax <- apply(cbind(x,y),1,max)
    mmin <- apply(cbind(x,y),1,min)
    mmax+log(1+exp(mmin-mmax))
  }
}

## RW MCMC Sampler

N_SA1 <- length(true_SA1_num_dwellings_by_DATUM_current[,1])
N_SA2 <- length(true_SA2_num_dwellings_by_DATUM_current[,1])

xacc_tot <- true_SA1_num_dwellings_by_DATUM_current*0
xacc_SA1 <- matrix(0,nrow=N_SA1,ncol=M)
xacc_SA2 <- matrix(0,nrow=N_SA2,ncol=M)
xhist_SA1 <- list()
xhist_SA2 <- list()
yhist_SA1 <- list()
zhist_SA1 <- list()

joint_covariates_data <- data.frame(joint_covariates)
joint_covariates_data$IRSAD_DECILE <- factor(joint_covariates_data$IRSAD_DECILE)

xtally <- 0
for (z in 1:10000000) {
  
  ### Regression Model
  
  if ((z %% 100000)==1) {
    
    asthma_fraction_logit <- log( true_SA1_num_dwellings_by_DATUM_current[,1] + 0.5) -  log( rowSums(true_SA1_num_dwellings_by_DATUM_current[,2:3]) + 0.5)
    asthma_fraction_var <- 1/(true_SA1_num_dwellings_by_DATUM_current[,1] + 0.5) + 1/(rowSums(true_SA1_num_dwellings_by_DATUM_current[,2:3]) + 0.5)
    regression_model <- lm(asthma_fraction_logit ~ IRSAD_DECILE+lstday+lst_diff+evi+tcb+tcw+ele+commdist+Nhighways+dist_industrial+dist_commercial ,data = joint_covariates_data, weight=1/asthma_fraction_var)
    vcov_regression <- vcov(regression_model)
    posterior <- mvrnorm(1,regression_model$coefficients,vcov_regression)
    regression_model$coefficients <- posterior 
    posterior_mean <- predict(regression_model)
    yhist_SA1[[length(yhist_SA1)+1]] <- regression_model$coefficients
    
    cat("regression happening ...")  
  }
    
  ### MCMC ACCEPT_REJECT
  
  proposed_location <- cbind(sample(1:N_SA1,1),sample(1:M,1))
  proposed_move <- sample(c(-1,1),1)
  current_value <- true_SA1_num_dwellings_by_DATUM_current[proposed_location]
  current_likelihood_diff <- 0
  
  xstop <- FALSE
  while (!xstop) {
    if (current_value==0 & proposed_move==-1) {
      #cat(" x ")
      xstop <- TRUE
    } else {
      
      # SA1 DATUM likelihood
      proposed_sa1_location <- proposed_location[1]
      current_value <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]]
      proposed_value <- current_value+proposed_move
      observed_value <- SA1_num_dwellings_by_DATUM_obs[proposed_sa1_location,proposed_location[2]]
      
      if (proposed_value==0 & observed_value>0) {
        #cat(" z ")
        xstop <- TRUE
      } else if (proposed_value==0 & observed_value==0) {
        proposed_likelihood <- 0
      } else if (proposed_value>0 & observed_value==0) {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),pnorm(2.5,proposed_value,error_sd,log.p=TRUE)))
      } else {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-proposed_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-proposed_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      if (current_value==0 & observed_value==0) {
        current_likelihood <- 0
      } else if (current_value>0 & observed_value==0) {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),pnorm(2.5,current_value,error_sd,log.p=TRUE)))
      } else {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-current_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-current_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      current_likelihood_diff <- current_likelihood_diff + proposed_likelihood - current_likelihood
      
      # SA1 ROW TOT likelihood
      current_value <- true_SA1_num_dwellings_current[proposed_sa1_location]
      proposed_value <- current_value+proposed_move
      observed_value <- SA1_num_dwellings_obs[proposed_sa1_location]
      
      if (proposed_value==0 & observed_value>0) {
        #cat(" Z ")
        xstop <- TRUE
      } else if (proposed_value==0 & observed_value==0) {
        proposed_likelihood <- 0
      } else if (proposed_value>0 & observed_value==0) {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),pnorm(2.5,proposed_value,error_sd,log.p=TRUE)))
      } else {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-proposed_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-proposed_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      if (current_value==0 & observed_value==0) {
        current_likelihood <- 0
      } else if (current_value>0 & observed_value==0) {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),pnorm(2.5,current_value,error_sd,log.p=TRUE)))
      } else {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-current_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-current_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      current_likelihood_diff <- current_likelihood_diff + proposed_likelihood - current_likelihood
    
      # Regression priors
      
      current_value_tot <- true_SA1_num_dwellings_current[proposed_sa1_location]
      proposed_value_tot <- current_value_tot+proposed_move
      current_value <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,1]
      if (proposed_location[2]==1) {proposed_value <- current_value+proposed_move} else {proposed_value <- current_value}
      
      logit_current_SA1 <- log(current_value+0.5)-log(current_value_tot-current_value+0.5)
      logitvar_current_SA1 <- 1/(current_value+0.5) + 1/(current_value_tot-current_value+0.5)
      logit_proposed_SA1 <- log(proposed_value+0.5)-log(proposed_value_tot-proposed_value+0.5)
      logitvar_proposed_SA1 <- 1/(proposed_value+0.5) + 1/(proposed_value_tot-proposed_value+0.5)
      
      proposed_priors <- dnorm(logit_proposed_SA1,posterior_mean[proposed_sa1_location],sqrt(logitvar_proposed_SA1),log=TRUE)
      current_priors <- dnorm(logit_current_SA1,posterior_mean[proposed_sa1_location],sqrt(logitvar_current_SA1),log=TRUE)
      
      current_likelihood_diff <- current_likelihood_diff + proposed_priors - current_priors
      
      # SA2 DATUM likelihood
      proposed_sa2_location <- sa1sa2codes[proposed_location[1]]
      current_value <- true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]]
      proposed_value <- current_value+proposed_move
      observed_value <- SA2_num_dwellings_by_DATUM_obs[proposed_sa2_location,proposed_location[2]]
      
      if (proposed_value==0 & observed_value>0) {
        #cat(" r ")
        xstop <- TRUE
      } else if (proposed_value==0 & observed_value==0) {
        proposed_likelihood <- 0
      } else if (proposed_value>0 & observed_value==0) {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),pnorm(2.5,proposed_value,error_sd,log.p=TRUE)))
      } else {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-proposed_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-proposed_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      if (current_value==0 & observed_value==0) {
        current_likelihood <- 0
      } else if (current_value>0 & observed_value==0) {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),pnorm(2.5,current_value,error_sd,log.p=TRUE)))
      } else {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-current_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-current_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      current_likelihood_diff <- current_likelihood_diff + proposed_likelihood - current_likelihood
      
      # SA2 ROW TOT likelihood
      current_value <- true_SA2_num_dwellings_current[proposed_sa2_location]
      proposed_value <- current_value+proposed_move
      observed_value <- SA2_num_dwellings_obs[proposed_sa2_location]
      
      if (proposed_value==0 & observed_value>0) {
        #cat(" R ")
        xstop <- TRUE
      } else if (proposed_value==0 & observed_value==0) {
        proposed_likelihood <- 0
      } else if (proposed_value>0 & observed_value==0) {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),pnorm(2.5,proposed_value,error_sd,log.p=TRUE)))
      } else {
        proposed_likelihood <- logsumexp(pnorm(-0.5,proposed_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-proposed_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-proposed_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      if (current_value==0 & observed_value==0) {
        current_likelihood <- 0
      } else if (current_value>0 & observed_value==0) {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),pnorm(2.5,current_value,error_sd,log.p=TRUE)))
      } else {
        current_likelihood <- logsumexp(pnorm(-0.5,current_value,error_sd,log.p=TRUE),logdiffexp(pnorm(-((observed_value-current_value)+0.5),0,error_sd,log.p = TRUE),pnorm(-((observed_value-current_value)-0.5),0,error_sd,log.p=TRUE)))
      }
      
      current_likelihood_diff <- current_likelihood_diff + proposed_likelihood - current_likelihood
      
      if (current_likelihood_diff < log(runif(1))) {
        xstop <- TRUE
      } else {
        
        xtally <- xtally+proposed_move
        
        true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]] <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]]+proposed_move
        true_SA1_num_dwellings_current[proposed_sa1_location] <- true_SA1_num_dwellings_current[proposed_sa1_location]+proposed_move
        
        true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]] <- true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]]+proposed_move
        true_SA2_num_dwellings_current[proposed_sa2_location] <- true_SA2_num_dwellings_current[proposed_sa2_location]+proposed_move
        
        xacc_SA1[proposed_sa1_location,proposed_location[2]] <- xacc_SA1[proposed_sa1_location,proposed_location[2]] + 1
        xacc_SA2[proposed_sa2_location,proposed_location[2]] <- xacc_SA2[proposed_sa2_location,proposed_location[2]] + 1
      }
      xstop <- TRUE
      
    }
    
    if ( (z %% 10000)==1 ) {
      cat("c: ",xtally," ",sum(true_SA1_num_dwellings_by_DATUM_current)/sum(SA2_num_dwellings_obs)," ")
    }
    
    if ( (z %% 10000)==1 ) {
      xhist_SA1[[length(xhist_SA1)+1]] <- true_SA1_num_dwellings_by_DATUM_current
      xhist_SA2[[length(xhist_SA2)+1]] <- true_SA2_num_dwellings_by_DATUM_current
    }
    if ( (z %% 100000)==1 ) {
      save(xhist_SA1,file="sa1hist_FOURCITIES.dat")
      save(xhist_SA2,file="sa2hist_FOURCITIES.dat")
      save(yhist_SA1,file="yhist_FOURCITIES.dat")
      save(zhist_SA1,file="zhist_FOURCITIES.dat")
    }
  }
}

save.image("postfit_ASTHMA_FOURCITIES.dat")

### Visualise and check results

xhist_SA1 <- xhist_SA1[-(1:250)]

# sa1 summaries
sa1_by_DATUM_count <- matrix(0,nrow=N_SA1,ncol=length(xhist_SA1))
for (i in 1:length(xhist_SA1)) {
  sa1_by_DATUM_count[,i] <- xhist_SA1[[i]][,1]
}
quantilelow <- function(x) {quantile(x,0.025)}
sa1_by_DATUM_lower <- apply(sa1_by_DATUM_count,1,quantilelow)
quantilehigh <- function(x) {quantile(x,0.975)}
sa1_by_DATUM_upper <- apply(sa1_by_DATUM_count,1,quantilehigh)
quantilemedian <- function(x) {quantile(x,0.5)}
sa1_by_DATUM_median <- apply(sa1_by_DATUM_count,1,quantilemedian)
sa1_by_DATUM_mean <- apply(sa1_by_DATUM_count,1,mean)

sa1_obs <- SA1_num_dwellings_by_DATUM_obs[,1]

sa1_2021_output <- rbind(sa1_2021_Perth,sa1_2021_Brisbane,sa1_2021_Melbourne,sa1_2021_Sydney)
sa1_2021_output$BMED <- sa1_by_DATUM_median
sa1_2021_output$BMEA <- sa1_by_DATUM_mean
sa1_2021_output$BLOW <- sa1_by_DATUM_lower
sa1_2021_output$BHIG <- sa1_by_DATUM_upper
sa1_2021_output$OBS <- sa1_obs

sa1_by_DATUM_frac <- matrix(0,nrow=N_SA1,ncol=length(xhist_SA1))
for (i in 1:length(xhist_SA1)) {
  sa1_by_DATUM_frac[,i] <- xhist_SA1[[i]][,1]/(0.00000001+rowSums(xhist_SA1[[i]]))
}
quantilelow <- function(x) {quantile(x,0.025)}
sa1_by_DATUM_lower <- apply(sa1_by_DATUM_frac,1,quantilelow)
quantilehigh <- function(x) {quantile(x,0.975)}
sa1_by_DATUM_upper <- apply(sa1_by_DATUM_frac,1,quantilehigh)
quantilemedian <- function(x) {quantile(x,0.5)}
sa1_by_DATUM_median <- apply(sa1_by_DATUM_frac,1,quantilemedian)
sa1_by_DATUM_mean <- apply(sa1_by_DATUM_frac,1,mean)

sa1_obs <- SA1_num_dwellings_by_DATUM_obs[,1]/(SA1_num_dwellings_obs+0.000001)

sa1_2021_output$BMEDF <- sa1_by_DATUM_median
sa1_2021_output$BMEAF <- sa1_by_DATUM_mean
sa1_2021_output$BLOWF <- sa1_by_DATUM_lower
sa1_2021_output$BHIGF <- sa1_by_DATUM_upper
sa1_2021_output$OBSF <- sa1_obs

xcol <- sa1_2021_output$BMEAF
xcol[xcol>0.3] <- 0.3
xcol <- (0.3-xcol)*0.666
plot(sa1_2021_output[1:4822,],col=hsv(xcol),border=NA)

ilogit <- function(x) {1/(1+exp(-x))}
sa1_by_DATUM_effect <- matrix(0,nrow=N_SA1,ncol=length(yhist_SA1))
for (i in 1:length(yhist_SA1)) {
  regression_model$coefficients <- yhist_SA1[[i]] 
  sa1_by_DATUM_effect[,i] <- ilogit(predict(regression_model))
}
quantilelow <- function(x) {quantile(x,0.025)}
sa1_by_DATUM_lower <- apply(sa1_by_DATUM_effect,1,quantilelow)
quantilehigh <- function(x) {quantile(x,0.975)}
sa1_by_DATUM_upper <- apply(sa1_by_DATUM_effect,1,quantilehigh)
quantilemedian <- function(x) {quantile(x,0.5)}
sa1_by_DATUM_median <- apply(sa1_by_DATUM_effect,1,quantilemedian)
sa1_by_DATUM_mean <- apply(sa1_by_DATUM_effect,1,mean)

sa1_2021_output$BMEDI <- sa1_by_DATUM_median
sa1_2021_output$BMEAI <- sa1_by_DATUM_mean
sa1_2021_output$BLOWI <- sa1_by_DATUM_lower
sa1_2021_output$BHIGI <- sa1_by_DATUM_upper

xcol <- sa1_2021_output$BMEAI
xcol[xcol>0.3] <- 0.3
xcol <- (0.3-xcol)*0.666
plot(sa1_2021_output[1:4822,],col=hsv(xcol),border=NA)

writeOGR(sa1_2021_output,".","sa1_asthma",driver="ESRI Shapefile",overwrite=TRUE)

