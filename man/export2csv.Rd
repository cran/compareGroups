\name{export2csv}
\alias{export2csv}

\title{
Exporting descriptives table to plain text (CSV) format
}
\description{
This function takes the result of \code{createTable} and exports the tables to plain text (CSV) format.
}
\usage{
export2csv(x, file, sep=",", ...)
}

\arguments{

  \item{x}{an object of class 'createTable'}

  \item{file}{file where table in CSV format will be written. Note that the the folder and the basename of the file must be written but not the extension (.csv). Also, another file with the extension '_appendix' is written with the available data table.}

  \item{sep}{character. The variable separator. If 'sep' is set to ',' it uses \code{\link[utils]{write.csv}}, and when is set to';' it uses \code{\link[utils]{write.csv2}}. Default value is ','.}
             
  \item{\ldots}{other arguments passed to \code{\link[utils]{write.table}}.}
  
}


\seealso{
  \code{\link{createTable}}, \code{\link{export2latex}}
}                               
\examples{

\dontrun{
data(myData)
ans<-compareGroups(y~.,myData)
export2csv(createTable(ans),file="c:/example/tables/table1")
}

}

\keyword{utilities}

