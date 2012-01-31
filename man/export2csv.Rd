\name{export2csv}
\alias{export2csv}

\title{
Exporting descriptives table to plain text (CSV) format
}
\description{
This function takes the result of \code{createTable} and exports the tables to plain text (CSV) format.
}
\usage{
export2csv(x, file, which.table="descr", sep=",", nmax = TRUE, ...)
}

\arguments{

  \item{x}{an object of class 'createTable'.}

  \item{file}{file where table in CSV format will be written. Note that the name of the folder and the basename of the file must be written but not the extension (.csv). Also, another file with the extension '_appendix' is written with the available data table.}

  \item{which.table}{character indicating  which table is printed. Possible values are 'descr', 'avail' or 'both' (partial matching allowed), exporting descriptives by groups table, available data table or both tables, respectively. Default value is 'descr'.}

  \item{sep}{character. The variable separator. If 'sep' is set to ',' it uses \code{\link[utils]{write.csv}}, and when is set to';' it uses \code{\link[utils]{write.csv2}}. Default value is ','.}
  
  \item{nmax}{logical, indicating whether to show the maximum number of responses ("n" of a complete, or least-missing, variable) available by group. Default value is TRUE.}
             
  \item{\ldots}{other arguments passed to \code{\link[utils]{write.table}}.}
  
}


\seealso{
  \code{\link{createTable}}, \code{\link{export2latex}}, \code{\link{export2html}}
}                               
                                                                                                                                   
\examples{

\dontrun{
require(compareGroups)
data(regicor)
res <- compareGroups(sex ~. -id-todeath-death-tocv-cv, regicor)
export2csv(createTable(res, hide.no = 'n'), file="table1")
}

}

\keyword{utilities}

