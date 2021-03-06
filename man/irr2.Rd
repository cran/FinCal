% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irr2.R
\name{irr2}
\alias{irr2}
\title{Computing IRR, the internal rate of return}
\usage{
irr2(cf, cutoff = 0.1, from = -1, to = 10, step = 1e-06)
}
\arguments{
\item{cf}{cash flow,the first cash flow is the initial outlay}

\item{cutoff}{threshold to take npv as zero}

\item{from}{smallest irr to try}

\item{to}{largest irr to try}

\item{step}{increment of the irr}
}
\description{
This function is the same as irr but can calculate negative value. This function may take a very long time. You can use larger cutoff and larger step to get a less precision irr first. Then based on the result, change from and to, to narrow down the interval, and use a smaller step to get a more precision irr.
}
\examples{
# irr2(cf=c(-5, 1.6, 2.4, 2.8))
# irr2(cf=c(-200, 50, 60, -70, 30, 20))
}
\seealso{
\code{\link{irr}}
}

