###############################
# Process ABC function output #
###############################

ABCoutDF = function(ABCout, observations=1000, sep=TRUE){
  
  ## Parameters and summaries for rejection sampling
  
  # remove NAs
  params = as.data.frame(ABCout[[1]])
  summaries = as.data.frame(ABCout[[2]])
  if (length(which(rowSums(is.na(summaries))>0))!=0){
    print(paste0(which(rowSums(is.na(summaries))>0), "summaries will be removed due to NAs."))
    params = params[-(which(rowSums(is.na(summaries))>0)),]
    summaries = na.omit(summaries)
  }
  
  
  ## Retain "True values"
  
  trues = sample(1:nrow(summaries), observations, replace=FALSE)
  paramsTrue = params[trues,]
  params = params[-trues,]
  summariesTrue = summaries[trues,]
  summaries = summaries[-trues,]
  
  if(sep){
    
    return(list(params, summaries, paramsTrue, summariesTrue))
    
  } else {
    
    return(list(cbind(params, summaries), cbind(paramsTrue, summariesTrue)))
    
  }
  
}