\name{compareGroups-package}
\alias{compareGroups-package}
\alias{compareGroups-package}
\docType{package}


\title{
Descriptive analysis by groups
}


\description{
This package is designed to read, interpret, summarise, display and
analyse epidemiological data. Can be used to create data summaries for
quality control, extensive reports for exploring data, including
publication-ready univariate or bivariate tables in plain text HTML and
LaTeX, and a figures to quickly visualise the distribution of your data.
Depending on the nature of the variable, various statistics are
calculated (mean, median, frequencies, incidences) and appropriate
tests are performed (t-test, Analysis of variance, Kruskal-Wallis, Fisher,
log-rank, ...). The latest version includes functionality for summarising
genetic data, and for performing low-level quality control of your database
(see radiograph()) to help identify data-entry errors. This version has
also been implemented in a web-interface to make compareGroups accessible
to non-R-users.
}


\details{
\tabular{ll}{
Package: \tab compareGroups\cr
Type: \tab Package\cr
Version: \tab 3.0\cr
Date: \tab 2014-07-07\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
Main functions: \code{\link{compareGroups}}, \code{\link{compareSNPs}}, \code{\link{createTable}}, \code{\link{missingTable}}, \code{\link{export2latex}}, \code{\link{export2html}}, \code{\link{export2csv}}, \code{\link{export2pdf}}, \code{\link{report}}, \code{\link{radiograph}}, \code{\link{cGroupsGUI}}, \code{\link{cGroupsWUI}}     
}


\author{
Main functions: Isaac Subirana <isubirana<at>imim.es>, Joan Vila <jvila<at>imim.es>, H\'ector Sanz <hsrodenas<at>gmail.com>, Gavin Lucas <gavin.lucas<at>cleargenetics.com> and David Gim\'enez <dgimenez1<at>imim.es>\cr

Web User Interface: Isaac Subirana <isubirana<at>imim.es>, Judith Pe\~nafiel <jpenafiel<at>imim.es>, Gavin Lucas <gavin.lucas<at>cleargenetics.com> and David Gim\'enez <dgimenez1<at>imim.es> \cr

Maintainer: Isaac Subirana <isubirana<at>imim.es>
}

\references{
  Isaac Subirana, Hector Sanz, Joan Vila (2014). Building Bivariate Tables: The compareGroups Package for R. Journal of Statistical Software, 57(12), 1-16. URL \url{http://www.jstatsoft.org/v57/i12/}.
}
 
\keyword{package}
