\name{regicor}
\alias{regicor}
\docType{data}

\title{
REGICOR cross-sectional data
}

\description{
This data comes from 3 different cross-sectional surveys of individuals representative of the population from a north-west Spanish province (Girona), REGICOR study.
}

\usage{data(regicor)}
\format{
  A data frame with 2294 observations on the following 21 variables.
  \describe{
    \item{\code{id}}{a labelled. Individual id}
    \item{\code{year}}{a factor with levels \code{1995} \code{2000} \code{2005}. Recruitment year}
    \item{\code{age}}{a labelled. Patient age at recruitment date}
    \item{\code{gender}}{a factor with levels \code{male} \code{female}. Gender}
    \item{\code{smoker}}{a factor with levels \code{Never smoker} \code{Current or former < 1y} \code{Never or former >= 1y}. Smoking status}
    \item{\code{sbp}}{a labelled. Systolic blood pressure}
    \item{\code{dbp}}{a labelled. Diastolic blood pressure}
    \item{\code{histbp}}{a factor with levels \code{Yes} \code{No}. History of hypertension}
    \item{\code{txhtn}}{a factor with levels \code{No} \code{Yes}. Hypertension (HTN) treatment}
    \item{\code{chol}}{a labelled. Total cholesterol (mg/dl)}
    \item{\code{hdl}}{a labelled. HDL cholesterol (mg/dl)}
    \item{\code{triglyc}}{a labelled. Triglycerides (mg/dl)}
    \item{\code{ldl}}{a labelled. LDL cholesterol (mg/dl)}
    \item{\code{histchol}}{a factor with levels \code{Yes} \code{No}. History of hypercholesterolemia}
    \item{\code{txchol}}{a factor with levels \code{No} \code{Yes}. Cholesterol treatment}
    \item{\code{height}}{a labelled. Height (cm)}
    \item{\code{weight}}{a labelled. Weight (Kg)}
    \item{\code{bmi}}{a labelled. Body mass index}
    \item{\code{phyact}}{a labelled. Physical activity (Kcal/week)}
    \item{\code{pcs}}{a labelled. Physical component summary}
    \item{\code{mcs}}{a labelled. Mental component summary}
  }
}

\details{
The variables collected in the REGICOR study were mainly cardiovascular risk factors (hundreds of variables were collected in the different questionnaires and blood measurements), but the variables present in this data set are just few of them. Also, for confidential reasons, the individuals in this data set are a 30\% aprox. random subsample of the original one. \cr \cr
Each variable of this data.frame contains label describing them in the attribute "label". \cr

For more informations, see the vignette.
}

\source{
For confidential reasons, the whole data set is not public available. 
However, for more information about the study this data comes from (REGICOR), visit the website \code{www.regicor.org}.
}


\examples{
data(regicor)
summary(regicor)
}
\keyword{datasets}
