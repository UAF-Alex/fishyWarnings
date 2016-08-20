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
#' nYear <- 2E2
#' Year <- 1:nYear
#' tripVal <- c(0.36, 0.21)
#' nBurn <- 50
#' sdU <- 0.05
#' 
#' # set up qE to increase harvest until collapse
#' qE_range <- c(tripVal[1]-0.2, tripVal[1]+0.05)
#' 
#' # goal here is to have harvest rate precisely hit certain
#' # values of qE during an integer year,
#' # while retaining linear change throughout
#' qEPoints <- c(qE_range[1], tripVal[2], tripVal[1], qE_range[2])
#' qEPoints <- qEPoints[qEPoints>=min(qE_range) & qEPoints<=max(qE_range)]
#' seq_arg0 <- c(as.list(range(qEPoints)), list(length.out=nYear))
#' yrProbs <- (qEPoints-min(qEPoints))/diff(range(qEPoints))
#' yrPoints <- quantile(Year, yrProbs)
#' qE <- approx(x=yrPoints, y=qEPoints, xout=Year)$y
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
#' B0 <- 400
#' rsl <- structure(
#' 	vector('list', nYear), class="rslist", .Names=Year
#' ) # for the plot.rslist method I wrote
#' for(i in 1:nYear){
#' 	B0 <- Burn(B0, n=100, qE=qE[i], sdU=0, accumulate=FALSE)
#' 	B_t <- Burn(B0, n=nBurn, qE=qE[i], sdU=sdU, accumulate=TRUE)
#' 	for(s in 1:ncol(warnMat)){
#' 		if(stats[s]=="redShift"){
#' 			rsl[[i]] <- ews(B_t, stat=stats[s])
#' 			warnMat[i,s] <- rsl[[i]]
#' 		}else{
#' 			warnMat[i,s] <- ews(B_t, stat=stats[s])
#' 		}
#' 	}
#' 	Bmu[i] <- mean(B_t)
#' }
#' 
#' 
#' # =============================
#' # = Plot Biomass and Warnings =
#' # =============================
#' par(mfrow=c(2,2), mar=c(1.75,1.75,0.5,1.75), ps=8, cex=1, mgp=c(0.75,0.15,0), tcl=-0.15)
#' plot(Year, Bmu, type='l')
#' abline(v=yrPoints, lty='dashed')
#' text_y <- min(Bmu)+yrProbs*diff(range(Bmu))
#' text(yrPoints, y=text_y, label=paste0("qE=", qEPoints), pos=c(4,4,2,2))
#' 
#' plot(Year, warnMat[,"sd"], col="forestgreen", type="l", ylab="Standard Deviation")
#' abline(v=yrPoints, lty='dashed')
#' plot(Year, warnMat[,"ac1"], col="blue", type='l', ylab="AR(1)")
#' abline(v=yrPoints, lty='dashed')
#' plot(rsl, xaxs='r')
#' par(new=TRUE)
#' plot(Year, warnMat[,"redShift"], col=adjustcolor("white",0.25), 
#' 	lwd=3, type='l', ylab="", xaxt='n', yaxt='n'
#' )
#' lines(Year, warnMat[,"redShift"], col=adjustcolor("black",0.5), lwd=0.5)
#' axis(side=4)
#' abline(v=yrPoints, lty='dashed')
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
