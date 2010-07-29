\name{myData}
\alias{myData}
\docType{data}
\title{Simulated data}
\description{
Simulated data with one grouping variable and three row-variables.
}
\usage{data(myData)}
\format{
  A data frame with 1000 observations on the following 4 variables.
  \describe{
    \item{\code{a}}{a numeric vector}
    \item{\code{b}}{a numeric vector}
    \item{\code{c}}{a factor with levels \code{1} \code{2}}
    \item{\code{y}}{a factor with levels \code{1995} \code{2000} \code{2005}}
  }
}

\examples{
data(myData)
head(myData)
summary(myData)
}
\keyword{datasets}
