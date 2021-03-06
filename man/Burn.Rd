% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Burn.R
\name{Burn}
\alias{Burn}
\title{Burn
Run burn-in phase for haddock model}
\usage{
Burn(x, n, ..., accumulate = FALSE)
}
\arguments{
\item{x}{initial biomass value}

\item{n}{number of burn-in years}

\item{...}{arguments to pass to \code{\link{Bstep}}}

\item{accumulate}{logical, save full burn-in series, or just final value?}
}
\value{
numeric, either the last biomass value, or the whole burn-in series
}
\description{
Burn
Run burn-in phase for haddock model
}

