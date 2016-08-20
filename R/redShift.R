#' Red Shift
#' Function to calculate the spectral slope; warning signal known as "red shift"
#' @param x numeric vector that is a regular (evently spaced) time series
#' @param ... not used
#' 
#' @return spectral slope
#' @export
redShift <- function(x, ...){
	sfit <- stats::spectrum(x, plot=FALSE, method="ar")
	spec <- c(log(sfit$spec))[-1]
	freq <- 1/(log10(sfit$freq))[-1] # log(1/sfit$freq)[-1]
	# cor(sfit$freq, sfit$spec, method="kendall")
	rs <- stats::lm(spec ~ freq)$coef[2]
	of <- order(freq)
	spec <- spec[of]
	freq <- freq[of]
	grps <- cut(freq, 2)
	# rs <- mean(spec[grps==levels(grps)[1]])/mean(spec[grps==levels(grps)[2]])
	# attr(rs, "y") <- freq
	names(spec) <- freq
	attr(rs, "z") <- spec
	attr(rs, "class") <- 'rs'
	return(rs)
}

#' Print rs
#' Print red shift output w/o the spec and freq
#' 
#' @param x output of \code{\link{redShift}}
#' @param ... to be consistent with print
#' @export
print.rs <- function(x, ...){
	attributes(x) <- NULL
	print(x)
	invisible(NULL)
}

#' Plot redShift List
#' Plot a list of outputs from redShift
#' 
#' @param x a list of outputs from \code{\link{redShift}}
#' @param ... parameters passed to graphics::image
#' @export
plot.rslist <- function(x, ...){
	requireNamespace("graphics", quietly = TRUE)
	
	zCol <- function (nCols, Z) { # took it from rbLib
		cols <- (grDevices::colorRampPalette(c("#000099", "#00FEFF", 
		    "#45FE4F", "#FCFF00", "#FF9400", "#FF3100")))(nCols)
		colVec_ind <- cut(Z, breaks = nCols)
		colVec <- cols[colVec_ind]
		return(colVec)
	}
	
	nYear <- length(x)
	Year <- 1:nYear
	nr <- length(attributes(x[[1]])$z)
	rsm <- sapply(x, function(x)attributes(x)$z)
	freqs <- as.numeric(rownames(rsm))
	cols <- zCol(256, seq(0,1,length.out=256))
	graphics::image(list(x=Year, y=freqs, z=t(rsm)), ylab="log(Frequency)", col=cols, ...)
	invisible(NULL)
}
