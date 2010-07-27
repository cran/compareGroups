\name{createTable}
\alias{createTable}
\alias{print.createTable}

\title{
Table of descriptives by groups: bivariate table 
}
\description{
This functions builds a 'compact' and 'nice' table with the descriptives by groups.
}
\usage{
createTable(x, hide = NA, digits = NA, type = NA, show.all = 
            TRUE, show.p.trend = FALSE, show.p.mul = FALSE, show.n = FALSE)
}

\arguments{

  \item{x}{an object of class 'compareGroups'}

  \item{hide}{an integer vector with as many components as row-variables. If it has length 1 it is recycled for all row-variables. Each component specifies which category must be hidden and not shown. This argument only applies to categorical row-variables, and for continuous row-variables it is ignored. If NA, all categories are displayed. Or a named vector specifying which row-variables 'hide' is applied, and for the rest of row-variables default value is applied. Default value is NA}

  \item{digits}{an integer vector with as many components as row-variables. If it has length 1 it is recycled for all row-variables. Each component specifies the number of significant decimals to be displayed. Or a named vector specifying which row-variables 'digits' is applied, and for the rest of row-variables default value is applied. Default value is NA which puts the 'appropiate' number of decimals (see vignette for further details)}

  \item{type}{an integer. 2 or NA - absolute and relative frequencies in brackets are shown; 1- only relative frequencies are displayed} 
  
  \item{show.all}{logical indicating whether the '[ALL]' column (all data without stratifying by groups) is displayed or not. Default value is TRUE}
  
  \item{show.p.trend}{logical indicating whether p-trend is displayed or not. It is always FALSE when there are less than 3 groups. Default value is FALSE.}
  
  \item{show.p.mul}{logical indicating whether the pairwise (between groups) comparisons p-values are displayed or not. It is allways FALSE when there are less than 3 groups. Default value is FALSE.}
  
  \item{show.n}{logical indicating whether number of individuals analysed for each row-variable are displayed or not in the 'descr' table. Default value is FALSE}
  
}


\value{

An object of class 'createTable', which contains a list of 2 matrix:

  \item{descr}{a character matrix of descriptives for all row-variables by groups and p-values in a 'compact' format}

  \item{avail}{a character matrix indicating the number of available data for each group, the type of variable (categorical, continuous-normal or continuous-non-normal) and the individuals selection made (if non selection 'ALL' is displayed.)}

  'print' returns these two tables. The first one, 'descr', in a 'nice' format.
  
  'update' modifies previous results from 'createTable'.
}


\seealso{
  \code{\link{compareGroups}}, \code{\link{export2latex}}, \code{\link{export2csv}}
}
\examples{

data(myData)

ans<-compareGroups(y~.,myData)
anstab<-createTable(ans)
anstab
update(anstab,show.n=TRUE)


}

\keyword{misc}

