#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector seqC(double firstVal, double secondVal, int length){
            double val = (secondVal - firstVal)/length;
            NumericVector sequence(length+1);
            sequence[0] = firstVal;

            for(int i=1; i < length+1; ++i){
              firstVal += val;
              sequence[i] = firstVal;
            };
            return sequence;
            }

// [[Rcpp::export]]
void rcpp_rcout(double v){
  // printing value of vector
  Rcout << "The value of v : " << v << "\n";

  // printing error message
  Rcerr << "Error message\n";
}

// [[Rcpp::export]]
DataFrame movmod(NumericVector paramsVar, NumericVector paramsFix, NumericVector startvalues, int steps, int days, float pie) { 
		//Assign data types to behavioural parameters
        double directionalPersistance = paramsVar(0);
		double BasketSize = paramsVar(1);
		double Lazyness = paramsVar(2);
		double foodRadius = paramsVar(3);
		
		//Assign data types to external parameters
		double directionalPersistanceBackway = paramsFix(0);
        double FlowerRichness = paramsFix(1);
		double TimeofReturn = paramsFix(2);
		double TimeofLeaving = paramsFix(3);
        double Consistency = paramsFix(4);
		 
		//assign types to Flower richness and Persistance which are later required by statistical rcpp sugar functions
		int foodfound;
		double probofsuccess;
		double meanSteplength;
		double length;
		double persOR;
		double movementLength;
		double homesickness;
		
		//create vector of sunlight and time during the day
		NumericVector sunlight = cos(seqC(pie, 3*pie, steps));
		sunlight = ifelse(sunlight < 0, 0, sunlight);
		NumericVector tsteps = seqC(0,24,(steps+2));
		int day = 0;
		
		//Vectors for output collection with length of final data frame
		int n = (days+1);
		int m = (steps+1);
		int ext = n*m;
		NumericVector xx(ext);
		NumericVector yy(ext);
		NumericVector fb(ext);
		NumericVector dd(ext);
		NumericVector tt(ext);
		NumericVector movemode(ext);
		NumericVector returntimes(ext);
		int countingDwarf = 0;
		
		//set daily parameters and start the day
        for(int j=0; j < days+1; ++j){
              day++;
			  
              double x = startvalues[0];
              double y = startvalues[1];
              double direction = startvalues[2];
              
              NumericVector out_x(steps+2);
              NumericVector out_y(steps+2);
			  NumericVector foodBasket(steps+2);
			  
			  int activity = 0;
			  int backway = 0;
			  int returntime = 0;
			  double food = 0;
			  double disttocamp = 0;
			  
			  foodBasket(0) = food;
              out_x(0) = x;
              out_y(0) = y;
				
				//Time of getting up today.
			  double pT;
			  pT = TimeofLeaving+fabs(R::rnorm(Lazyness, 0.3));
			  
              for(int i=0; i < steps+1; ++i) {
				  countingDwarf++;
				  dd(countingDwarf-1) = day;
				  xx(countingDwarf-1) = out_x(i);
				  yy(countingDwarf-1) = out_y(i);
				  fb(countingDwarf-1) = foodBasket(i);
				  tt(countingDwarf-1) = tsteps(i);
				  returntimes(countingDwarf-1) = returntime;
				  
				  //if get up time is reached, activity status is changed to collecting
				  if(pT < tsteps(i)){
					activity = 1;
					}
					
				// if sun has already set, the animal stays in the camp and goes into resting mode.
				  if((sunlight(i) == 0) & (x == 0) & (y == 0)){
					activity = 0;
					backway = 0;
					movemode(countingDwarf-1) = 0;}
					
				  if(activity == 1){
					  // set animal movement to collecting mode
					  movemode(countingDwarf-1) = 1;
					  
					  // between a certain radius from the camp and the further away the animal is, the higher is the probability to find food.
					  disttocamp = sqrt(x*x + y*y);
					  probofsuccess = (disttocamp / foodRadius - 0.1);
					  if (probofsuccess > 1) {probofsuccess = 1;} //
					  
					  // check if the animal finds food in this step and modify movementlength as a function of food.
					  foodfound = R::rbinom(1, probofsuccess);
					  if (foodfound == 1){
						food = fabs(R::rlnorm(0, 0.7));
						meanSteplength = 5;
						movementLength = fabs(R::rnorm(meanSteplength, 1));
						
					// if no food found, meansteplength is two times larger than  if found was found.
					  } else {
						  food = 0;
						  meanSteplength = 10;
						  movementLength = fabs(R::rnorm(meanSteplength, 1));
					  }
					  
					  //add food to basket if still space in the basket
					  if(foodBasket(i) < BasketSize){
						foodBasket(i+1) = foodBasket(i)+food;}
					  else {foodBasket(i+1) = foodBasket(i);}
						  
					  // probability of going home with foodBasket(i) full.
					  double pFB = R::dnorm(foodBasket(i), BasketSize, 2, 0);
					  // probability of going home because of time. Because this is most of the time 0, it's pS + pFB.
					  double pS = R::dnorm(tsteps(i), TimeofReturn, Consistency, 0)+pFB;
					  homesickness = pFB+pS;
					  if(homesickness > 1){homesickness = 1;}
					  
					  // Sample if animal starts backway movement
					  if(backway == 0){
					  backway = R::rbinom(1,homesickness);}
					  else {backway = 1;}
					  
					  if(backway == 1){
						  movemode(countingDwarf-1) = 2;
						  // move animal towards basecamp if outside a radius of 5 
						  if((fabs(x) > 5) | (fabs(y) > 5)){
						    persOR = R::rnorm(0, directionalPersistanceBackway);
							  direction = atan2(y, x) + pie + persOR;
							  length = 2*R::rexp(movementLength); //animal moves faster on the backway.
							  x = x + cos(direction)*length;
							  out_x(i+1) = x;
							  y = y + sin(direction)*length;
							  out_y(i+1) = y;}
						  // if close enough to basecamp, move there directly. 
						  else {x = startvalues[0];
							  y = startvalues[1];
							  out_x(i+1) = x;
							  out_y(i+1) = y;
							  foodBasket(i+1) = 0;
							  backway = 0;
							  returntime++;}
						// if the animal is not on the backway, move as in collecting mode.
					  } else {persOR = R::rnorm(0, directionalPersistance);
					    direction = direction + persOR;
					    length = R::rexp(movementLength);
						x = x + cos(direction)*length;
						out_x(i+1) = x;
						y = y + sin(direction)*length;
						out_y(i+1) = y;}
				  // if animal is not active, keep location and status
				  } else {
					
					movemode(countingDwarf-1) = 0;
					out_x(i+1) = x;
					out_y(i+1) = y;
					foodBasket(i+1)= 0 ;}
					
			  }
			  }
			DataFrame df_out = DataFrame::create(Named("time") = tt, Named("day")=dd, Named("x")=xx, Named("y")=yy, Named("movementmode")=movemode, Named("foodBasket")=fb, Named("Returntimes")=returntimes);
			return df_out;
}

//[[Rcpp::export]]
double Quantiles(NumericVector vec, double prob){
	// this function returns the quantile "prob" of a vector's entries.
	
	NumericVector wvec = abs(clone(vec));
	std::sort(wvec.begin(), wvec.end());
	return wvec[vec.size()*(prob-0.000001)];
}

//[[Rcpp::export]]
NumericVector diffLag(NumericVector vec, int Lag, bool left){
	// this function is equal to the function diff() only with a lag to both sides of the index.
	
  int l = vec.length();
	NumericVector vecLagged(l);
	if (left){
		for (int i=Lag; i < l; i++){
			vecLagged(i) = vec(i) - vec(i-Lag);
		}
	} else {
		for (int i=Lag; i < l; i++){
			vecLagged(i) = vec(i) - vec(i+Lag);
		}
	}
	return vecLagged;
}

// [[Rcpp::export]]
NumericVector summaryStatistics(NumericMatrix data){
	// This functions returns a vector of speciefied summaryStatistics, computed from the simulated data returned by movmod.
	
	NumericVector time = data(_,0);
	NumericVector day = data(_,1);
	NumericVector xobs = data(_,2);
	NumericVector yobs = data(_,3);
	NumericVector mode = data(_,4);
	
	double meandisplacement;
	double sddisplacement;
	double meandisplacement3;
	double sddisplacement3;
	double meanturning;
	double sdturning;
	double meanturning3;
	double sdturning3;
	double meanturningCollection;
	double sdturningCollection;
	double xquantile50;
	double xquantile90;
	double meandayreturns;
	double meandisttocamp;
	double mediandisttocamp;
	double maxdisttocamp;
	double displacementbydistance;
	double meandailydistance;
	double meantimeofleaving;
	
	// stepwise displacement 
	meandisplacement = mean(sqrt(diff(xobs)*diff(xobs)+diff(yobs)*diff(yobs)));
	sddisplacement = sd(sqrt(diff(xobs)*diff(xobs)+diff(yobs)*diff(yobs)));
	
	// displacement in 3 steps
	meandisplacement3 = mean(sqrt(diffLag(xobs, 3, true)*diffLag(xobs, 3, true)+diffLag(yobs, 3, true)*diffLag(yobs, 3, true)))/3;
	sddisplacement3 = sd(sqrt(diffLag(xobs, 3, true)*diffLag(xobs, 3, true)+diffLag(yobs, 3, true)*diffLag(yobs, 3, true)))/3;
	
	// turning angle
	NumericVector ydiff = diff(yobs);
	NumericVector xdiff = diff(xobs);
	int l = ydiff.length();
	NumericVector meanturningV(l);
	for (int i = 0; i < l; ++i) {
	  meanturningV(i) = atan2(ydiff(i), xdiff(i));
	}
	meanturning = mean(abs(diff(meanturningV)));
	sdturning = sd(abs(diff(meanturningV)));
	
	//turning angle in 3 steps
	NumericVector ydiff3 = diffLag(yobs, 3, true);
	NumericVector xdiff3 = diffLag(xobs, 3, true);
	int l3 = ydiff.length();
	NumericVector meanturningV3(l3);
	for (int i = 0; i < l3; ++i) {
	  meanturningV3(i) = atan2(ydiff3(i), xdiff3(i));
	}
	meanturning3 = mean(abs(diff(meanturningV3)))/3;
	sdturning3 = sd(abs(diff(meanturningV3)))/3;
	
	// turning angle in collection mode
	
	//Quantiles of distance to basecamp
	xquantile50 = Quantiles(xobs, 0.5);
	xquantile90 = Quantiles(xobs, 0.9);
	
	// Times of return to basecamp
	int l2 = xobs.length();
	int returns = 0;
	NumericVector xlags = diffLag(xobs, 1, true);
	NumericVector ylags = diffLag(yobs, 1, true);
	
	for (int i = 0; i < l2; ++i){
		if((xobs(i) == 0) & (yobs(i) == 0) & (xlags(i) != 0) & (ylags(i) != 0)){
			returns++;}
	}
	meandayreturns = returns/max(day);
	
		// complete daily walking distance
	double distance = 0;
	for(int i=0; i < l2; ++i){
		distance = distance + sqrt(xlags(i)*xlags(i)+ylags(i)*ylags(i));
	}
	meandailydistance = distance/max(day);
	
	//mean distance to camp
	NumericVector disttocamp(l2);
	for (int i = 0; i < l2; ++i){
		disttocamp(i) = sqrt(xobs(i)*xobs(i) + yobs(i)*yobs(i));
	}
	meandisttocamp = mean(disttocamp);
	mediandisttocamp = Quantiles(disttocamp, 0.5);
	
	//maximum distance to camp
	maxdisttocamp = max(disttocamp);
	
	// mean steplength if farther away from basecamp than mean distance.
	NumericVector meandisplacementV = sqrt(diff(xobs)*diff(xobs)+diff(yobs)*diff(yobs));
	NumericVector meandisplatdist(l2-1);
	for (int i = 0; i < l2-1; ++i){
		
		if (disttocamp(i) > meandisttocamp){
			meandisplatdist(i) = meandisplacementV(i);
		}else{
			meandisplatdist(i) = NA_REAL;
		}
	}
	displacementbydistance = mean(na_omit(meandisplatdist));
	
	// leaving time in the morning
	NumericVector xlags2 = diffLag(xobs, 2, true);
	NumericVector ylags2 = diffLag(yobs, 2, true);
	NumericVector xlags20 = diffLag(xobs, 20, true);
	NumericVector ylags20 = diffLag(yobs, 20, true);
	double leavingtime = 0;
	
	for (int i = 0; i < l2-1; ++i){
		if ((xobs(i) == xlags2(i)) & (yobs(i) == ylags2(i)) & (xobs(i) == xlags20(i)) & (yobs(i) == ylags20(i)) & ((xobs(i) != 0) & (yobs(i) != 0 ))){
			leavingtime = leavingtime + time(i);
		}
	}
	meantimeofleaving = leavingtime/max(day);
	
	NumericVector SumStats = NumericVector::create(Named("meandisplacement")= meandisplacement, Named("sddisplacement") = sddisplacement, Named("meandisplacement3") = meandisplacement3, Named("sddisplacement3") = sddisplacement3, Named("meanturning") = meanturning, Named("sdturning") = sdturning,Named("meanturning3") = meanturning3, Named("sdturning3") = sdturning3, Named("xquantile50") = xquantile50, Named("xquantile90") = xquantile90, Named("meandayreturns") = meandayreturns, Named("meandisttocamp") = meandisttocamp,Named("mediandisttocamp")=mediandisttocamp, Named("maxdisttocamp") = maxdisttocamp, Named("displacementbydistance") = displacementbydistance, Named("meanleavingtime") = meantimeofleaving, Named("meandailydistance") = meandailydistance);
	return SumStats;
}

//[[Rcpp::export]]
NumericMatrix observationModel(NumericMatrix realData, double sd){
	// this function takes the simulated paths from movmod and turns the data into "observation" data by adding a measurement error and randomly removing about 30% of the datapoints.
	
	int n = realData.rows();
	int m = realData.cols()+2;
	NumericVector x = realData(_,2);
	NumericVector y = realData(_,3);
	NumericVector xobs(n);
	NumericVector yobs(n);
	NumericMatrix m_out(n, m);
	
	for (int i = 0; i < n; i++){
		xobs(i) = R::rnorm(x(i), sd);
		if ( (xobs(i)-floor(xobs(i))) > 0.7){
			xobs(i) = NA_REAL;
		}
		yobs(i) = R::rnorm(y(i), sd);
	}
	for (int i = 0; i < m-2; i++){
	  m_out(_,i) = realData(_,i);
	}
	
	m_out(_,(m-2)) = xobs;
	m_out(_,(m-1)) = yobs;
	
	return m_out;
}


//[[Rcpp::export]]
NumericMatrix DFtoNM(DataFrame x) {
	// This function takes a data frame and converts it to a numeric Matrix
	//thx to stackoverflow.com/questions/24352208/best-way-to-convert-dataframe-to-matrix-in-rcpp
  int nRows=x.nrows();  
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }  
  return y;
}


//[[Rcpp::export]]
List ABC(int nruns, int sumstats, NumericVector fixparams, NumericVector startvals, int steps, int days, double pie){
	// This function simulates "observation" data n times and calculates the summary statistics from them.
	
	NumericMatrix paramssample(nruns, 4);
	NumericMatrix simulatedprediction;
	double sd;
	NumericVector simulatedsummary;
	NumericMatrix simulatedobservation;
	NumericMatrix allsummaries(nruns, sumstats);
	
	
	for (int i = 0; i < nruns; i++){
		//sampling the five varying parameters from wide uniform distributions.
	  paramssample(i, 0) = R::runif(0,1); // directional persistance
	  paramssample(i, 1) = R::runif(1,20); // BasketSize
	  paramssample(i, 2) = R::runif(0,2); // Lazyness
	  paramssample(i, 3) = R::runif(10,60); // FoodRadius
	  sd = R::runif(0,2);
	  NumericVector varparams = paramssample(i,_);
	  
	  simulatedprediction = DFtoNM(movmod(varparams, fixparams, startvals, steps, days, pie)); // simulate the true movement path - returns NumericMatrix
	  simulatedobservation = observationModel(simulatedprediction, sd); // simulate error and missing data on true path
	  simulatedsummary = summaryStatistics(simulatedobservation);
	  allsummaries(i,_) = simulatedsummary;
	}
	
	List L = List::create(Named("paramcombination") = paramssample, Named("SumStats") = allsummaries);
	return L;
	}

//[[Rcpp::export]]
NumericVector rejection(NumericVector Eobs, NumericMatrix Esim){
	
	int n = Esim.rows();
	NumericVector dists(n);
	
	for (int i = 0; i < n; i++){
	  
		dists(i) = sqrt(sum((Eobs - Esim(i,_))*(Eobs - Esim(i,_))));
	}
	
	return dists;
}

//[[Rcpp::export]]
NumericVector normalize01(NumericVector params, bool backtransform){
	
	int n = params.length();
	NumericVector paramsNorm(n);
	
	if (backtransform == true){
		for (int i=0; i < n; i++){
			paramsNorm(i) = (params(i) + min(params)) * (max(params)+min(params) );
	}
	} else {
		for (int i=0; i < n; i++){
			paramsNorm(i) = (params(i) - min(params)) / (max(params)-min(params));
	}
	}
	return paramsNorm;
}