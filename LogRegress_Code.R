############################################################
# John S. Mancini
############################################################
# Kaggle AXA driver telematics analysis:
###
# Kaggle Goal: "telematic fingerprint" capable of 
#  distinguishing when a trip was driven by a given driver.
###
# Data and Challenge: 2737 folders each containing 200 car 
#  trips where each trip has an arbitary number of xy 
#  meter coordiantes as a function of time.  Of the 200 
#  car trips in each folder the majority of the trips are 
#  made by one driver. The challenge is to give 
#  the probablity that a trip was made by the one driver.
# Kaggle Evaluation: ROC 
############################################################
# Model Type: Logistical Regression Supervised 
###
# Model Notes: The model takes the trips of i-1 randomly selected
#  drivers to be targeted to 0 and then the 200 trips of 
#  interest targeted to 1. The model is then evaluated over
#  the 200 trips of interst.  
###   
#  Feature Notes: Features were chosen by measuring how their 
#  consideration effected the ROC curve result computed by 
#  Kaggle and by considering the results of Mann–Whitney U tests.
############################################################
############################################################
### 
#Random Seed
set.seed(2)
###
require("zoo")
###
# Computes 23 features
# time: total trip time
# startEndDist: distance between xy start and end coordinates
# totalDist: cumulative driving distance
# stopTime: time spent with 0 velocity
# maxVel: max velocity
# maxAcc: max acceleration 
# maxDec: max deceleration
# quantVel: velocity distribution (5 values) 
# quantAcc: acceleration distribution (5 values)  
# quantDec: deceleration distribution (5 values)  
###
featureCompute <- function(trip){
  dist <- sqrt(diff(trip$x)^2 + diff(trip$y)^2)
  cumuTrip <- cumsum(dist[!dist > mean(dist) + sd(dist) * 5])
  veloc <- diff(cumuTrip,2)/2
  veloc <- veloc[!veloc > mean(veloc) + sd(veloc) * 5]
  acc <- diff(veloc,2)/2
  time <- nrow(trip)
  startEndDist <- sqrt((trip$x[1]-trip$x[time])^2+(trip$y[1]-trip$y[time])^2)
  totalDist <- cumuTrip[length(cumuTrip)]
  stopTime <- length(veloc[veloc<0.01])
  #
  maxVel <- max(veloc)
  maxAcc <- max(acc)
  maxDec <- (-1)*min(acc)
  #
  quantVel <- quantile(veloc[veloc>0.01], probs = seq(0, 1, 0.25))
  quantAcc <- quantile(acc[acc>0],probs = seq(0, 1, 0.25))
  quantDec <- quantile(abs(acc[acc<0]),probs = seq(0, 1, 0.25))
  #
  feat <- c(time,startEndDist,totalDist,quantVel,quantDec,quantAcc,maxVel,maxDec,maxAcc,stopTime)
  feat[is.na(feat)] <-0
  feat[is.infinite(feat)] <- 0
  return(feat)
  }
###
# evalutate features for 200 trips in a folder
###
runDriver <- function(driver,target){
  names(target) = "target"
  features <- NULL
  dirPath <- paste0("/Users/John/Documents/drivers/", driver, '/')
  for(i in 1:200){
    trip <- read.csv(paste0(dirPath,i, ".csv"))
    temp <- featureCompute(trip)
    names(temp) <- make.names(c(1:length(temp)))
    drive <- c(target,temp)
    features <- rbind(features,drive)
  }
  return(features)
}
############################################################
############################################################
# Main Program
############################################################
############################################################
drivers <- list.files("/Users/John/Documents/drivers/")
i <- 7
temp <- rep(1,i)
while(length(unique(temp))<i){temp <- sample(drivers,i)}
randomDrivers1 <- temp[1:i-1]
randomDriverPlus <- temp[length(temp)]
####
refData0 <- NULL
refData1 <- NULL
# generate target "0" data
for(driver in randomDrivers1){
  feature <- runDriver(driver,0)
  refData0 <- rbind(refData0,feature)
}
namesVector <- make.names(c(1:ncol(refData0)))
submission <- NULL
pb <- txtProgressBar(min=1,max=length(drivers),style=3)
prog <- 1
# generate submission data
for(driver in drivers){
  driverData <- NULL
  refData1 <- refData0
  # ensure target "0" does not contain the driver to be
  # be tested
  if(length(randomDrivers1[randomDrivers1==driver]!=0)){
    refData1 <- NULL
    tempRandomDrivers <- randomDrivers1
    tempRandomDrivers[which(tempRandomDrivers==driver)] <- randomDriverPlus
    for(driver2 in tempRandomDrivers){
      feature <- runDriver(driver2,0)
      refData1 <- rbind(refData1,feature)
   	}	
  }
  # generate target "1" data
  driverData <- runDriver(driver,1)
  train <- rbind(driverData, refData1)
  train <- as.data.frame(train)
  # build logistical regression model
  model <- glm(target ~ .,data=train,family=binomial("logit"))
  predict <- predict(model,as.data.frame(driverData), type = "response")
  labels <- sapply(1:200,function(x) paste0(driver,'_', x))
  measurement <- cbind(labels,predict)
  submission <- rbind(submission,measurement)
  setTxtProgressBar(pb,prog)
  prog <- prog+1
}
close(pb)
#write submission data 
colnames(submission) = c("driver_trip","prob")
write.csv(submission, "logistical_regression_solution.csv", row.names=FALSE, quote=FALSE)
