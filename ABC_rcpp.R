library(Rcpp)
library(ggplot2)

# source functions from cpp and R

sourceCpp("~/Sc_Master/SelectedTopics/ABC_applied/CPPcode/model.cpp")

source("~/Sc_Master/SelectedTopics/ABC_applied/functions/processABCoutput.R")
#source("~/Sc_Master/SelectedTopics/ABC_applied/functions/introplots.R")

# ==============================#
# Exemplary movement simulation #
# ==============================#

# Specify parameters for a "true" path

directionalPersistance = 0.3 # runif(1, 0, 1)
BasketSize = 15 # runif(1, 1, 20)
Lazyness = 1.0 # runif(1,0,0.1)
FoodRadius = 30 #runif(1,10,60)

fixparameters = c(directionalPersistanceBackway = 0.3, FlowerRichness = 5, TimeofReturn = 17, ConsistencyofReturn = 0.3, TimeofLeaving = 5.5)
startvalues = c(0,0,0)
days = 0
tsteps = 600

data = movmod(paramsVar=c(directionalPersistance, BasketSize, Lazyness, FoodRadius), paramsFix = fixparameters , startvalues = startvalues, steps=tsteps, days = days, pie=pi)

save(data, file="~/Sc_Master/SelectedTopics/ABC_applied/Rdata/data.Rdata")
load("~/Sc_Master/SelectedTopics/ABC_applied/Rdata/data.Rdata")

pdf("~/Sc_Master/SelectedTopics/ABC_applied/plots/simulations.pdf", width=5, height=5)
ggplot(data) + 
  geom_path(aes(x=x, y=y, group=day), color="grey60") + 
  geom_point(aes(x=x, y=y, group=day, colour=time), alpha=0.7, size=2) +
  scale_colour_gradient(name="Time \nof day", low="grey70", high = "grey20")  +
  theme_light()  + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour = "black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black"), legend.text = element_text(size=12), legend.title = element_text(size=12)) #+ xlim(c(-90,90)) + ylim(-90,90)
dev.off()


# Observation Model 

## Assume we have recorded the test data. In fact, let's do it a bit harder. Assume we observe with error, and our measurement device has a problem - if the x-values have a digit larger than 0.7, we get an NA

obsdata = observationModel(as.matrix(data), 1)
obsdataGG = as.data.frame(obsdata)
names(obsdataGG) = c(names(data), "xobs", "yobs")
obsdataGG = na.omit(obsdataGG)

pdf("~/Sc_Master/SelectedTopics/ABC_applied/plots/observations.pdf", width=5, height=5)
ggplot(obsdataGG)  + 
  geom_path(aes(x=xobs, y=yobs, group=day), color="grey60") +
  geom_point(aes(x=xobs, y=yobs, group=day, colour=time), alpha = 0.7, size=2) + scale_colour_gradient(name="Time \nof Day", low="peachpuff", high = "red") + theme_light() + coord_equal(ratio=1) + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black")) # + xlim(c(-90,90)) + ylim(-90,90)
dev.off()
# + geom_point(aes(x=x, y=y, group=day, colour="Simulated Path"), size=0.9) 

## calculate a set of summary statistics from the observed data.

dataSummary = summaryStatistics(obsdata)
dataSummary
 
save(dataSummary, file="~/Sc_Master/Selected Topics/ABC_applied/Rdata/dataSummary.Rdata")

#################
# Stochasticity #
#################

stoch = NULL
plotlist = list()

for (i in 1:500){
  dat = movmod(paramsVar=c(directionalPersistance, BasketSize, Lazyness, FoodRadius), paramsFix = fixparameters , startvalues = startvalues, steps=tsteps, days = days, pie=pi)
  obsdat = observationModel(as.matrix(dat), sd=1)
  obsdatGG = as.data.frame(obsdat)
  names(obsdatGG) = c(names(dat), "xobs", "yobs")
  obsdatGG = na.omit(obsdatGG)
  
  if(i < 5){
    p = ggplot(obsdatGG)  + 
      geom_path(aes(x=xobs, y=yobs, group=day), color="grey60") +
      geom_point(aes(x=xobs, y=yobs, group=day), alpha = 0.7, size=2, color="black") + scale_color_manual(name="", values=c("red","grey40")) + theme_light() + coord_equal(ratio=1) + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black")) + xlim(c(-90,90)) + ylim(-90,90)
    #plotlist[[i]] = p
  }
  
  datsum = summaryStatistics(obsdat)
  stoch = rbind(stoch, c(datsum, i))
}


for(i in 1:4){
  filename = paste0("~/Sc_Master/SelectedTopics/ABC_applied/plots/simulationsN",i,".pdf")
  pdf(filename, width=5, height=5)
  print(plotlist[[i]])
  dev.off()
}

stoch = na.omit(as.data.frame(stoch))
#save(stoch, file= "~/Sc_Master/SelectedTopics/ABC_applied/Rdata/stochAccepted.Rdata")
hist(stoch$meandisplacement, breaks=30)

pdf("~/Sc_Master/SelectedTopics/ABC_applied/plots/showstochasticity-rejected.pdf", width=5, height=5)
ggplot(stoch) + geom_histogram(aes(x=meandisplacement), colour="black", bins=40) + theme_light() + xlab("Mean displacement") + ylab("Count") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black")) + geom_vline(aes(xintercept=dataSummary[1]), col="red") + xlab("Mean steplength") + xlim(c(0,3)) #+ ylim(c(0,40))
dev.off()

#================== # 
# Fitting the model #
#================== #

## Generate 100000 paths
n = 100000
nstats = length(dataSummary)

ABCout = ABC(n, nstats, fixparams = fixparameters , startvalues, tsteps, days, pi)
save(ABCout, file="~/8787/ABC/Rdata/ABCout.Rdata")
load("~/8787/ABC/Rdata/ABCout.Rdata")

# Convert ABC output.
## Parameters and Summaries for rejections sampling
ABCoutL = ABCoutDF(ABCout = ABCout, observations = 1000, sep=TRUE)
params = ABCoutL[[1]]
summaries = ABCoutL[[2]]
## 1000 "true" observations
paramsTrue = ABCoutL[[3]]
summariesTrue = ABCoutL[[4]]


## Summary Statistics Aggregation ##

# Fit random forests to aggregate a subset of 25.000 summary statistics for each parameter

require(ranger)
require(parallel)

simsRF = sample(1:nrow(summaries), 25.000, replace=FALSE)
paramsRF = params[simsRF,]
params = params[-simsRF,]
summariesRF = summaries[simsRF,]
summaries = summaries[-simsRF,]

Par = numeric(nrow(summariesRF))
summaries = cbind(Par, summariesRF) # column to be filled up with each parameter for model building.
simulatedEstimatesRF = matrix(0, nrow = nrow(paramsRF), ncol = ncol(paramsRF))


for(i in 1:ncol(paramsRF)){
  
  summariesRF[,1] = paramsRF[,i]
  names(summariesRF)[1] = paste0("Par",i)
  assign(paste0("rf"), ranger(formula = formula(summariesRF), data = summariesRF, importance = "impurity", num.threads = 12))
  save(rf, file=paste0("~/8787/ABC/Rdata/rf",i,".Rdata"))
  simulatedEstimatesRF[,i] = rf$predictions
  summariesTrue = cbind(summariesTrue, predict(rf, data = summariesTrue, type="response")$predictions)
  names(summariesObs)[length(summariesTrue)] = paste0("Par",i)
  summariesRF = cbind(summariesRF, simulatedEstimatesRF[,i])
  names(summariesRF)[length(summariesRF)] = paste0("Par",i)
  rm(rf)
}


#save simulated Estimates
save(simulatedEstimatesRF, file="~/8787/ABC/Rdata/simulatedEstimatesRF.Rdata")
save(summariesTrue, file="~/8787/ABC/Rdata/summariesTrue.Rdata")

#load("~/Sc_Master/SelectedTopics/ABC_applied/Rdata/simulatedEstimatesRF.Rdata")
#load("~/Sc_Master/SelectedTopics/ABC_applied/Rdata/observedEstimates.Rdata")

# save observed estimates
observedEstimates = summariesTrue[,16:19]

 
######################
# Rejection Sampling #
######################


## Aggregate the remaining summary statistics with random forest.


simulatedEstimates = matrix(0, ncol = ncol(params), nrow = nrow(params))

for (i in 1:ncol(params)){
  load(paste0("~/8787/ABC/Rdata/rf",i,".Rdata"))
  pred = predict(rf, data=summaries, type="response")
  simulatedEstimates[,i] = pred$predictions
  summaries = cbind(summaries, simulatedEstimates[,i])
  names(summaries)[ncol(summaries)] = paste0("Par",i)
  rm(rf)
}

save(simulatedEstimates, file="~/Sc_Master/SelectedTopics/ABC_applied/Rdata/simulatedEstimates.Rdata")
#load("~/Sc_Master/SelectedTopics/ABC_applied/Rdata/simulatedEstimates.Rdata")


# Accept parameter combinations, that generate the 0.5 closest parameter values for each parameter. Euclidean distance is used as a distance measure to summarize 4 parameters.

acceptedDF = NULL
for(i in 1:nrow(observedEstimates)){
  dists = rejection((as.numeric(observedEstimates[i,])), as.matrix((simulatedEstimates)))
  accepted = as.data.frame(simulatedEstimates[which(dists < quantile(dists, prob=0.001, na.rm=TRUE)),])
  accepted$ind = i
  rbind(acceptedDF, accepted)
}
save(acceptedDF, file="~/8787/ABC/Rdata/acceptedDF.Rdata")
