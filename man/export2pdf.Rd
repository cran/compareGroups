\name{export2pdf}
\alias{export2pdf}

\title{
Exports tables to PDF files.
}

\description{
This function creates automatically a PDF with the table. Also, the LaTeX code is stored in the specified file.
}

\usage{
export2pdf(x, file, compile = TRUE, openfile = TRUE, ...)
}

\arguments{

  \item{x}{an object of class 'createTable' or that inherits it.}

  \item{file}{character specifying the PDF file (without the extension) resulting after compiling the LaTeX code corresponding to the table specified in the 'x' argument. LaTeX code is also stored in the same folder with the same name but .tex extension. When 'compile' argument is FALSE, only .tex file is saved.}

  \item{compile}{logical indicating whether tex file is compiled using \code{\link[tools]{texi2pdf}} function. Default value is TRUE.}
  
  \item{openfile}{logical indicating whether to open the compiled pdf file or not. It uses the \code{\link[Hmisc]{sys}} function and does not apply when 'compile' argument is FALSE. Default value is TRUE.}  
             
  \item{\ldots}{Arguments passed to \code{\link{export2latex}}.}
  
}


\note{

  To make the .tex file be compiled, some LaTeX compiler such as Miktex must be installed. Also, the tex file must include the following LaTeX packages:
  - babel \cr
  - longtable  \cr
  - multirow    \cr
  - lscape     \cr
  - geometry \cr
  - inputenc \cr

}


\seealso{
  \code{\link{createTable}}, \code{\link{export2latex}}, \code{\link{export2csv}}, \code{\link{export2html}}, \code{\link{report}}
} 


\examples{

\dontrun{

require(compareGroups)
data(regicor)

 # example on an ordinary table
res <- createTable(compareGroups(year ~ . -id, regicor), hide = c(sex=1), hide.no = 'no')
export2pdf(res, "table", size="small")

}

}

\keyword{utilities}
