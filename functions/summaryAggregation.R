#######################
# Summary Aggregation #
#######################

summaryAggregation = function(Sobs, Pobs, Ssim, Psim, size=NULL, type="rf", prediction=FALSE, filePath){
  
  if(prediction==TRUE){
    
  }else{
    
    Par = numeric(nrow(Ssim))
    Ssim = cbind(Par, Ssim) # column to be filled up with each parameter for model building.
    simulatedEstimatesRF = matrix(0, nrow = nrow(Psim), ncol = ncol(Psim))
    
    
    for(i in 1:ncol(Psim)){
      
      Ssim[,1] = Psim[,i]
      names(Ssim)[1] = paste0("Par",i)
      
      assign("rf", ranger(formula = formula(Ssim), data = Ssim, importance = "impurity"))
      save(rf, file=paste0("rf",i,".Rdata"))
      
      simulatedEstimatesRF[,i] = rf$predictions
      Sobs = cbind(Sobs, predict(rf, data = Sobs, type="response")$predictions)
      names(Sobs)[length(Sobs)] = paste0("Par",i)
      Ssim = cbind(Ssim, simulatedEstimatesRF[,i])
      names(Ssim)[length(Ssim)] = paste0("Par",i)
      
      rm(rf)
    }
    
    save(simulatedEstimates, file = "~/simulatedEstimatesRF.Rdata")
    save(Sobs, file= "~/summariesTrue.Rdata")
  }
  return(list(simulatedEstimates, Sobs))
  
}