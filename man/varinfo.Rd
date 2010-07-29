\name{varinfo}
\alias{varinfo}
\alias{varinfo.compareGroups}

\title{
Variable names and labels extraction
}

\description{
This functions builds and prints a table with the variable names and their labels.
}

\usage{
varinfo(x, ...)
\method{varinfo}{compareGroups}(x, ...)
}

\arguments{
  \item{x}{an object of class 'compareGroups'}
  \item{\dots}{other arguments currently ignored}
}

\details{
This function is useful when several or all variables are labelled. Then, by default, in the descriptives tables, the labels appear instead of the variable names. It may be necessary to remember the 'original'  variable names in a 'quicker way' in order to be easier to modify some options of the analysis or to change some of the variables included in the tables.
}

\value{
A 'matrix' with two columns
\item{Orig varname}{actual variable name in the 'data.frame' or in the 'parent environment'.}
\item{Shown varname}{names of the variable shown in the resulting tables.}
}

\note{
If a variable has no "label" attribute, then the 'original name' is the same as the 'shown name'.
The first variable in the table corresponds to the grouping variable. To label non-labelled variables, use the function \code{\link[Hmisc]{label}}.
}

\seealso{
\code{\link{compareGroups}}, \code{\link{createTable}}
}

\examples{
data(myData)
label(myData$a)<-"variable a"
label(myData$b)<-"variable b"
label(myData$c)<-"variable c"
label(myData$y)<-"grouping variable"
ans<-compareGroups(y~.,data=myData)
createTable(ans)
varinfo(ans)
}

\keyword{utilities}

