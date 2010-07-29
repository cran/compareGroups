\name{export2latex}
\alias{export2latex}

\title{
Exporting descriptives table to Latex format
}
\description{
This function takes the result of \code{createTable} and exports the tables to LaTeX format.
}
\usage{
export2latex(x, file,...)
}

\arguments{

  \item{x}{an object of class 'createTable'}

  \item{file}{file where table in LaTeX format will be written. Note that the the folder and the basename of the file must be written but not the extension (.tex). Also, another file with the extension '_appendix' is written with the available data table.}

  \item{\ldots}{other arguments passed to \code{\link[utils]{write.table}}.}
  
}


\seealso{
  \code{\link{createTable}}, \code{\link{export2csv}}
}                               
\examples{

\dontrun{
data(myData)
ans<-compareGroups(y~.,myData)
export2latex(createTable(ans),file="c:/example/tables/table1")
}

}

\keyword{utilities}

