#' Red Shift
#' Function to calculate the spectral slope; warning signal known as "red shift"
#' @param x numeric vector that is a regular (evently spaced) time series
#' @param ... not used
#' 
#' @return spectral slope
#' @export
redShift <- function(x, ...){
	sfit <- stats::spectrum(x, plot=FALSE, method="pgram")
	spec <- c(log(sfit$spec))[-1]
	freq <- log(1/sfit$freq)[-1]
	# cor(sfit$freq, sfit$spec, method="kendall")
	stats::lm(spec ~ freq)$coef[2]
}