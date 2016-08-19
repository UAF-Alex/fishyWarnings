#' EWS
#' Calculate Early Warning Signals
#' 
#' @param x numeric vector that is a regular time series for which to calculate early warnign statistic
#' @param stat length 1 character that is the early warning statistic
#' @param ... arguments to be passed to function specified by \code{stat}
#' 
#' @return an early warning summary statistic
#' @seealso this is just a wrapper for \code{\link{sd}}, \code{\link{ac1}}, \code{\link{redShift}}
#' @export
#' 
#' @examples
#' # ==================
#' # = Set up Options =
#' # ==================
#' nYear <- 5E2
#' Year <- 1:nYear
#' tripVal <- c(0.36, 0.21)
#' nBurn <- 50
#' sdU <- 0.05
#' 
#' # set up qE to increase harvest until collapse
#' qE_range <- c(tripVal[1]-0.2, tripVal[1]+0.1)
#' seq_arg <- c(as.list(qE_range), list(length.out=nYear))
#' qE <- do.call(seq, seq_arg)
#' 
#' 
#' # ===========================================
#' # = Simulate Biomass and Calculate Warnings =
#' # ===========================================
#' # For each year, simulate nBurn years
#' # B in the top-level year is the mean of the nBurn sims
#' # Early warnings calculated from the nBurn, so no rolling window
#' warnMat <- matrix(NA, ncol=3, nrow=nYear, dimnames=list(NULL, c("sd","ac1","redShift")))
#' stats <- colnames(warnMat)
#' Bmu <- numeric(nYear)
#' B0 <- Burn(400, n=nBurn, qE=qE[1], sdU=0, accumulate=FALSE)
#' for(i in 1:nYear){
#' 	B_t <- Burn(B0, n=nBurn, qE=qE[i], sdU=sdU, accumulate=TRUE)
#' 	for(s in 1:ncol(warnMat)){
#' 		warnMat[i,s] <- ews(B_t, stat=stats[s])
#' 	}
#' 	Bmu[i] <- mean(B_t)
#' 	B0 <- Bmu[i]
#' }
#' 
#' 
#' # =============================
#' # = Plot Biomass and Warnings =
#' # =============================
#' dev.new(width=6, height=6)
#' par(mfrow=c(2,2), mar=c(1.75,1.75,0.5,1.75), ps=8, cex=1, mgp=c(0.75,0.15,0), tcl=-0.15)
#' plot(Year, Bmu, type='l')
#' qECrit2 <- Year[which.min(abs(qE-tripVal[2]))]
#' abline(v=qECrit2, lty='dashed')
#' text(qECrit2, y=1*max(Bmu), label=paste0("qE = ", tripVal[2]), pos=2)
#' 
#' warnMat_scale <- scale(warnMat)
#' plot(Year, warnMat_scale[,"sd"], col="forestgreen", type="l", ylab="Standard Deviation")
#' abline(v=qECrit2, lty='dashed')
#' plot(Year, warnMat_scale[,"ac1"], col="blue", type='l', ylab="AR(1)")
#' abline(v=qECrit2, lty='dashed')
#' plot(Year, warnMat_scale[,"redShift"], col="red", type='l', ylab="redShift")
#' abline(v=qECrit2, lty='dashed')
ews <- function(x, stat=c("sd","ac1","redShift"), ...){
	stat <- match.arg(stat)
	if(stat=="sd"){
		out <- stats::sd(x)
	}
	if(stat=="ac1"){
		out <- ac1(x)
	}
	if(stat=="redShift"){
		out <- redShift(x, ...)
	}
	return(out)
}
