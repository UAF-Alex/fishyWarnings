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
#' redVec0 <- rollWarn(Bvec, stat="redShift", win=min(50, nYear/5)) # holds the full spectrum
#' redList <- attributes(redVec0)$rsl # this gets the full spectrum formatted for plotting
#' redVec <- sapply(redVec0, function(x)x[[1]]) # this gets just the spectral slope
#' 
#' # =======================
#' # = Plot B and Warnings =
#' # =======================
#' par(mfrow=c(2,2), mar=c(1.75,1.75,0.5,1.75), ps=8, cex=1, mgp=c(0.75,0.15,0), tcl=-0.15)
#' plot(Year, Bvec, type='l')
#' abline(v=yrPoints, lty='dashed')
#' text_y <- min(Bvec)+yrProbs*diff(range(Bvec))
#' text(yrPoints, y=text_y, label=paste0("qE=", qEPoints), pos=c(4,4,2,2))
#' 
#' plot(Year, sdVec, col="forestgreen", type="l", ylab="Standard Deviation")
#' abline(v=yrPoints, lty='dashed')
#' plot(Year,ac1Vec, col="blue", type='l', ylab="AR(1)")
#' abline(v=yrPoints, lty='dashed')
#' plot(redList, xaxs='r')
#' par(new=TRUE)
#' plot(Year, redVec, col=adjustcolor("white",0.5), lwd=3, type='l', ylab="", xaxt='n', yaxt='n')
#' lines(Year, redVec, col=adjustcolor("black",0.5), lwd=0.5)
#' axis(side=4)
#' mtext("redShift", side=4, line=0.75)
#' abline(v=yrPoints, lty='dashed')
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
		rsl <- structure(vector('list', (length(x)-(win-1)))) # for the plot.rslist method I wrote
		redShift2 <- function(x){
			saveInd <- which(sapply(rsl, function(x)is.null(x)))[1]
			rsx <- redShift(x)
			rsl[[saveInd]] <<- rsx
			return(rsx)
		}
		out <- unname(zoo::rollapplyr(x, FUN=redShift2, width=win, fill=NA))
		
		rsNA <- function(){
			o <- NA
			tz <- attributes(rsl[[1]])$z
			tz[] <- NA
			attr(o, "names") <- "freq"
			attr(o, "z") <- tz
			attr(o, "class") <- "rs"
			return(o)
		}
		rsl0 <- replicate(win-1, rsNA(), simplify=FALSE)
		rslc <- c(rsl0, rsl)
		attr(rslc, "class") <- "rslist"
		attr(out, 'rsl') <- rslc
	}
	return(out)
}