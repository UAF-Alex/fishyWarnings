#' Burn
#' Run burn-in phase for haddock model
#' 
#' @param x initial biomass value
#' @param n number of burn-in years
#' @param ... arguments to pass to \code{\link{Bstep}}
#' @param accumulate logical, save full burn-in series, or just final value?
#' 
#' @return numeric, either the last biomass value, or the whole burn-in series
#' @export
Burn <- function(x, n, ..., accumulate=FALSE){
	if(accumulate){
		x0 <- x
		x <- numeric(n)
		x[1] <- x0
		for(i in 2:n){
			x[i] <- Bstep(x[i-1], ...)
		}
	}else{
		for(i in 2:n){
			x <- Bstep(x, ...)
		}
	}
	return(x)
}