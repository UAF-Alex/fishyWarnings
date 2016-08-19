#' Roll Warn
#' Calculate early warnings statistics over a rolling window
#' 
#' @param x numeric vector over which to calculate statistics; should be a regularly spaced time series
#' @param stat name of statistic to use; see \code{\link{ews}}
#' @param win window length
#' 
#' @return numeric vector of a rolling window statistic
#' @seealso \code{\link{ews}}
#' @import RcppRoll zoo
#' @export
#' 
#' @examples
#' # ==========================================
#' # = Set up simulation and analysis options =
#' # ==========================================
#' nYear <- 5E2
#' Year <- 1:nYear
#' tripVal <- c(0.36, 0.21)
#' nBurn <- 50
#' sdU <- 0.05
#' qE_range <- c(tripVal[1]-0.2, tripVal[1]+0.1)
#' seq_arg <- c(as.list(qE_range), list(length.out=nYear))
#' qE <- do.call(seq, seq_arg)
#' 
#' # ==============================
#' # = Claculate time series of B =
#' # ==============================
#' B0 <- 400
#' B0 <- Burn(B0, n=nBurn, qE=qE[1], sdU=0)
#' Bvec <- c(B0, rep(NA, nYear-1))
#' for(i in 2:nYear){
#' 	Bvec[i] <- Bstep(B=Bvec[i-1], qE=qE[i], sdU=sdU)
#' }
#' 
#' # ==================================
#' # = Calculate rolling window stats =
#' # ==================================
#' sdVec <- rollWarn(Bvec, stat='sd', win=min(50, nYear/5))
#' ac1Vec <- rollWarn(Bvec, stat='ac1', win=min(50, nYear/5))
#' redVec <- rollWarn(Bvec, stat="redShift", win=min(50, nYear/5))
#' 
#' # =======================
#' # = Plot B and Warnings =
#' # =======================
#' dev.new(width=6, height=6)
#' par(mfrow=c(2,2), mar=c(1.75,1.75,0.5,1.75), ps=8, cex=1, mgp=c(0.75,0.15,0), tcl=-0.15)
#' plot(Year, Bvec, type='l')
#' qECrit2 <- Year[which.min(abs(qE-tripVal[1]))]
#' abline(v=qECrit2, lty='dashed')
#' text(qECrit2, y=1*max(Bvec), label=paste0("qE = ", tripVal[1]), pos=2)
#' 
#' plot(Year, sdVec, col="forestgreen", type="l", ylab="Standard Deviation")
#' abline(v=qECrit2, lty='dashed')
#' plot(Year,ac1Vec, col="blue", type='l', ylab="AR(1)")
#' abline(v=qECrit2, lty='dashed')
#' plot(Year, redVec, col="red", type='l', ylab="redShift")
#' abline(v=qECrit2, lty='dashed')
rollWarn <- function(x, stat=eval(formals(ews)$stat), win=15){
	requireNamespace("RcppRoll", quietly=TRUE)
	requireNamespace("zoo", quietly=TRUE)
	stat <- match.arg(stat)
	if(stat=="sd"){
		out <- RcppRoll::roll_sdr(x, n=win) # is fast
	}
	if(stat=="ac1"){
		# option 1
		# l2 <- embed(x, 2)
		# out <- RollingWindow::RollingCorr(l2[,2], l2[,1], window=win) # is fast
		# out <- c(NA, out)
		# option 2
		out <- unname(zoo::rollapplyr(x, FUN=ac1, width=win, fill=NA))
	}
	if(stat=="redShift"){
		out <- unname(zoo::rollapplyr(x, FUN=redShift, width=win, fill=NA))
	}
	return(out)
}