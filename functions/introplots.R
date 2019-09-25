############################
# Generate a range of plots#
############################

introPlots = function(dp=0.3, BS=15, L=1.0, FR= 30, days=0, steps=600, filepath ="~/Sc_Master/SelectedTopics/ABC_applied/plots", coordlimits = 140){
  
  setwd(filepath)
  
  directionalPersistance = dp
  BasketSize = BS
  Lazyness = L
  FoodRadius = FR
  
  fixparameters = c(directionalPersistanceBackway = 0.3, FlowerRichness = 4, TimeofReturn = 17, ConsistencyofReturn = 0.2, TimeofLeaving = 5.5)
  startvalues = c(0,0,0)
  days = days
  tsteps = steps
  
  data = movmod(paramsVar=c(directionalPersistance, BasketSize, Lazyness, FoodRadius), paramsFix = fixparameters , startvalues = startvalues, steps=tsteps, days = days, pie=pi)
  
  p = ggplot(data) + 
    geom_path(aes(x=x, y=y, group=day), color="grey60") + 
    geom_point(aes(x=x, y=y, group=day, colour=time), alpha=0.7, size=2) +
    scale_colour_gradient(name="Time \nof day", low="grey70", high = "grey20")  +
    theme_light() + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour = "black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black"), legend.text = element_text(size=12), legend.title = element_text(size=12)) + xlim(c(-coordlimits,coordlimits)) + ylim(-coordlimits,coordlimits)
  pdf("simulations.pdf", width=5.5, height=5)
  print(p)
  dev.off()
  
  # Observation Model 
  
  obsdata = observationModel(as.matrix(data), 1)
  obsdataGG = as.data.frame(obsdata)
  names(obsdataGG) = c(names(data), "xobs", "yobs")
  obsdataGG = na.omit(obsdataGG)
  
  
  p = ggplot(obsdataGG)  + 
    geom_path(aes(x=xobs, y=yobs, group=day), color="grey60") +
    geom_point(aes(x=xobs, y=yobs, group=day, colour=time), alpha = 0.7, size=2) + scale_colour_gradient(name="Time \nof Day", low="peachpuff", high = "red") + theme_light() + coord_equal(ratio=1) + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black"), legend.text = element_text(size=12), legend.title = element_text(size=12))  + xlim(c(-coordlimits,coordlimits)) + ylim(-coordlimits,coordlimits)
  pdf("observations.pdf", width=5.5, height=5)
  print(p)
  dev.off()
  
  ## calculate a set of summary statistics from the observed data.
  
  dataSummary = summaryStatistics(obsdata)
  dataSummary
  
  #################
  # Stochasticity #
  #################
  
  stoch = NULL
  plotlist = list()
  
  for (i in 1:500){
    dat = movmod(paramsVar=c(dp, BS, L, FR), paramsFix = fixparameters , startvalues = startvalues, steps=tsteps, days = days, pie=pi)
    obsdat = observationModel(as.matrix(dat), sd=1)
    obsdatGG = as.data.frame(obsdat)
    names(obsdatGG) = c(names(dat), "xobs", "yobs")
    obsdatGG = na.omit(obsdatGG)
    
    if(i < 8){
      p = ggplot(obsdatGG)  + 
        geom_path(aes(x=xobs, y=yobs, group=day), color="grey60") +
        geom_point(aes(x=xobs, y=yobs, group=day, colour=time), alpha = 0.7, size=2) + scale_colour_gradient(name="Time \nof day", low="grey70", high = "grey20") + theme_light() + coord_equal(ratio=1) + xlab("X") + ylab("Y") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black"), legend.text = element_text(size=12), legend.title = element_text(size=12)) + xlim(c(-coordlimits,coordlimits)) + ylim(-coordlimits,coordlimits)
      plotlist[[i]] = p
    }
    
    datsum = summaryStatistics(obsdat)
    stoch = rbind(stoch, c(datsum, i))
  }
  
  for(i in 1:7){
    filename = paste0("simulationsN",i,".pdf")
    pdf(filename, width=5.5, height=5)
    print(plotlist[[i]])
    dev.off()
  }
  
  stoch = na.omit(as.data.frame(stoch))
  save(stoch, file= "~/Sc_Master/SelectedTopics/ABC_applied/Rdata/stochAccepted.Rdata")
  
  p = ggplot(stoch) + geom_histogram(aes(x=meandisplacement), colour="black", bins=40) + theme_light() + xlab("Mean displacement") + ylab("Count") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black")) + geom_vline(aes(xintercept=dataSummary[1]), col="red") + xlab("Mean steplength") + xlim(c(0,15))
  pdf("showstochasticity-accepted.pdf", width=5.5, height=5)
  print(p)
  dev.off()
  
  ####
  # Example for rejection
  ####
  
  stoch = NULL
  
  dp_n = 0.7
  BS_n = 3
  L_n = 0.2
  FR_n = 50
  
  for (i in 1:500){
    dat = movmod(paramsVar=c(dp_n, BS_n, L_n, FR_n), paramsFix = fixparameters , startvalues = startvalues, steps=tsteps, days = days, pie=pi)
    obsdat = observationModel(as.matrix(dat), sd=1)
    obsdatGG = as.data.frame(obsdat)
    names(obsdatGG) = c(names(dat), "xobs", "yobs")
    obsdatGG = na.omit(obsdatGG)
    
    
    datsum = summaryStatistics(obsdat)
    stoch = rbind(stoch, c(datsum, i))
  }
  
  stoch = na.omit(as.data.frame(stoch))
  
  p = ggplot(stoch) + geom_histogram(aes(x=meandisplacement), colour="black", bins=40) + theme_light() + xlab("Mean displacement") + ylab("Count") + theme(axis.title = element_text(size = 16, colour="black"), axis.text = element_text(size=14, colour="black"), aspect.ratio = 1, axis.line = element_line(colour="black")) + geom_vline(aes(xintercept=dataSummary[1]), col="red") + xlab("Mean steplength") + xlim(c(0,15))
  pdf("showstochasticity-rejected.pdf", width=5.5, height=5)
  print(p)
  dev.off()
}

introPlots()
