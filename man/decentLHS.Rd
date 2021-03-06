% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decentLHS.R
\name{decentLHS}
\alias{decentLHS}
\title{Create decent LHS}
\usage{
decentLHS(n, d, ndes, max.time)
}
\arguments{
\item{n}{Number of points}

\item{d}{Number of dimensions}

\item{ndes}{Max number of random designs to generate}

\item{max.time}{Max amount of time}
}
\value{
Matrix with rows of points
}
\description{
Generate random Latin hypercube samples with a
given budget and return the best one according to
the MaxPro criterion.
}
\examples{
decentLHS(20,2, ndes=10)
}
