#' Schaefer Model
#' Schaefer Model for Haddock biomass change. Default parameter values are for haddock biomass in Spencer and COllie (1997)
#' 
#' @param B biomass of haddock in previous time step
#' @param r intrinsic growth rate of haddock
#' @param K carrying capacity of haddock
#' @param ... not used
#' 
#' @details
#' Fit is from 1976-1993, which authors reported as being "reasonable" (there was difficulty in getting a good fit to data from this model, particularly compared to Steele and Henderson model)
#' 
#' @seealso \code{\link{SH}} \code{\link{Bstep}}
#' 
#' @export
schaefer <- function(B, r=0.40, K=129.0, ...){
	r*B*(1- B/K)
}

#' Steele and Henderson Model
#' Steele and Henderson model for haddock biomass. Default parameters are empirically fitted values from Spencer and Collie (1997).
#' 
#' @param B biomass of haddock in previous time step
#' @param r intrinsic growth rate of haddock
#' @param K carrying capacity of haddock
#' @param D maximum rate of predation in type III predator functional response
#' @param A predator saturation in type III predator functional response
#' 
#' @details
#' The default arguments are the parameter values from Table 1 of Spencer and Collie 1997
#' 
#' @seealso \code{\link{schaefer}} \code{\link{Bstep}}
#' 
#' @export
SH <- function(B, r=0.76, K=520.2, D=28.2, A=27.9, ...){
	r*B*(1-B/K)-((D*B^2)/(A^2 + B^2))
}

#' Bstep
#' Function to increment haddock biomass 1 year
#' 
#' @param B biomass in kt
#' @param qE catchability*effort = F (harvest rate); F*B = C, catch
#' @param sdU standard deviation of the random shock; set to 0 for deterministic simulation
#' @param dt within-season size of time step; should be a number > 0 and <= 1
#' @param fB function of B; should be a character of either "SH" or "schaefer", corresponding to one of the two models
#' @param ... arguments passed to fB
#' 
#' @details
#' Follows equation 1 in Spencer and Collie 1997.
#' 
#' @note In the example below, the equilibrium value (average between 2020 and 2040) for F=0.18 is different under low-noise and high-noise scenarios. Below the simulation is as in the paper, but if the noise level is decreased (or if simulated deterministically), then the equiblirum is much closer to the cases of F=0.21 and F=0.24. I think this is a noise-induced transition. Cool!
#' 
#' @examples
#' 
#' # Make Figure 5a
#' makeB <- function(){c(13.8, rep(NA, 49))} # 13.8 kt is from pg 2922, second column, penultimate paragraph; it's the starting value in 1993
#' Year <- seq(1993, length.out=50)
#' qE <- c(0, 0.06, 0.12, 0.18, 0.21, 0.24)
#' Bvec <- replicate(length(qE), makeB())
#' for(j in 1:length(qE)){
#' 	for(i in 2:nrow(Bvec)){
#' 		Bvec[i,j] <- Bstep(Bvec[i-1,j], qE[j])
#' 	}
#' }
#' ltys <- c("solid", "dotted", "dashed", "longdash", "twodash", "dotdash")
#' ylim <- range(Bvec, na.rm=TRUE)
#' plot(Year, Bvec[,1], type='l', lty=ltys[1], ylim=ylim, ylab="Biomass (kt)", xlab="time", main="STH model")
#' for(j in 2:ncol(Bvec)){
#' 	lines(Year, Bvec[,j], lty=ltys[j])
#' }
#' legend("topleft", paste("F", qE, sep="="), lty=ltys)
#' 
#' @export
Bstep <- function(B, qE=0.15, sdU=0.1, dt=0.1, fB=c("SH", "schaefer"), ...){
	fB <- match.fun(match.arg(fB))
	nT <- 1/dt
	C <- min(B, qE*B)
	B <- B - C
	for(i in 1:nT){
		fB_val <- fB(B=(B), ...)
		B <- B + fB_val*dt
	}
	u <- rnorm(1, mean=0, sd=sdU)
	B*exp(u)
}




