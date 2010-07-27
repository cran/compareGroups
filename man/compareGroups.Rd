\name{compareGroups}
\alias{compareGroups}
\alias{compareGroups.default}
\alias{compareGroups.formula}
\alias{print.compareGroups}
\alias{plot.compareGroups}
\alias{update.compareGroups}
\alias{summary.compareGroups}
\alias{print.summary.compareGroups}

\title{
Descriptives by groups
}


\description{
This function performs descriptives by groups for several variables. Depending on the nature of these variables, different descriptive statistics are calculated (mean, median, frequencies) and different tests are computed as appropriate (t-test, ANOVA, Kruskall-Wallis, Fisher,...).
}

\usage{
compareGroups(X, ...)
\method{compareGroups}{default}(X, y, selec = NA, method = 1, alpha = 0.05, min.dis = 
          5, max.ylev = 5, max.xlev=10, include.label=TRUE, ...)
\method{compareGroups}{formula}(X, data, subset, na.action= NULL, include.label=TRUE, ...)
\method{plot}{compareGroups}(x, z=1.5 ,n.breaks="Sturges", ...)
}

\arguments{

  \item{X}{either a data.frame or a matrix (then method 'compareGroups.default' is called), or a formula (then method 'compareGroups.formula' is called). When 'X' is a formula, it must be an object of class "formula" (or one that can be coerced to that class). Right side of ~ must have the terms in an additive way.}
  
  \item{y}{a vector variable that distinguishes the groups. It must be either a numeric, character or factor.}
  
  \item{selec}{a character vector with as many components as row-variables. If it has length 1 it is recycled for all row-variables. Every component of 'selec' is an expression character that will be evaluated to select the individuals to be analysed for every row-variable. Or a named vector specifying which row-variables 'selec' is applied, and for the rest of row-variables default value is applied. Default value is NA, which means that all individuals are analysed (no subsetting).}
  
  \item{method}{an integer vector with as many components as row-variables. If it has length 1 it is recycled for all row-variables. It only applies for continuous row-variables (for factor row-variables it is ignored). Possible values are: 1 - forces to be analysed as normal-distributed; 2 - forces to be analysed as continuous non-normal; 3 - forces to be analysed as categorical; and NA - performs a Shapiro-Wilks test to decide between normal or non-normal. Or a named vector specifying which row-variables 'method' is applied, and for the rest of row-variables default value is applied. Default value is 1.}
  
  \item{alpha}{double between 0 and 1. Significance threshold for the \code{\link[stats]{shapiro.test}} normality test for continuous row-variables. Default value is 0.05.}
  
  \item{min.dis}{an integer. If a non-factor row-variable contains less than 'min.dis' different values and 'method' argument is set to NA, then it will be converted to a factor. Default value is 5.}
  
  \item{max.ylev}{an integer indicating the maximum number of levels of grouping variable ('y'). If 'y' contains more than 'max.ylev' levels, then the function 'compareGroups' produces an error. Default value is 5.}

  \item{max.xlev}{an integer indicating the maximum number of levels when the row-variable is a factor. If the row-variable is a factor (or converted to a factor if it is a character, for example) and contains more than 'max.xlev' levels, then it is removed from the analysis and a warning is printed. Default value is 10.}
    
  \item{data}{an optional data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.}
  
  \item{subset}{an optional vector specifying a subset of individuals to be used in the computation process. It is applied to all row-variables. 'subset' and 'selec' are added in the sense of '&' to be applied in every row-variable.}
  
  \item{na.action}{a function which indicates what should happen when the data contains NAs. The default is NULL, and that is equivalent to \code{\link[stats]{na.pass}}, which means no action. Value \code{\link[stats]{na.exclude}} can be useful if it is desired to removed all individuals with some NA in any variable.}

  \item{include.label}{logical, indicating whether or not variable labels have to be shown in the results. Default value is TRUE}

  \item{x}{an object of class 'compareGroups'}  

  \item{z}{double. threshold limits to be placed in the deviation from normality plot. It is considered that too many points beyond this threshold indicates that current variable is far to be normal-distributed. Default value is 1.5.}

  \item{n.breaks}{same as argument 'breaks' of \code{\link[graphics]{hist}}} 
  
  \item{\dots}{further arguments passed to 'compareGroups.default' or other methods}

}

\details{

Depending whether the row-variable is considered as continuous normal-distributed (1), continuous non-normal distributed (2) or categorical (3), the following descriptives and tests are performed: \cr
  1- mean, standard deviation and t-test or ANOVA \cr
  2- median, 1st and 3rd quartiles, and Kruskall-Wallis test \cr
  3- or absolute and relative frequencies and chi-squared or exact Fisher test when the expected frequencies is less than 5 in some cell\cr

When there are more than 2 groups, it also performs pairwise comparisons adjusting for multiple testing (Tukey when row-variable is normal-distributed and Benjamini & Hochberg method otherwise), and computes p-value for trend. 
The p-value for trend is computed from the Pearson test when row-variable is normal and from the Spearman test when it is continuous non normal. If the row-variable is categorical, the p-value for trend is computed as \code{1-pchisq(cor(as.integer(x),as.integer(y))^2*(length(x)-1),1)}, where 'x' is the row-variable and 'y' is the grouping variable.

See the vignette to see more detailed examples illustrating the use of this function and their methods.

}


\value{

 An object of class 'compareGroups'. 

  'print' returns a table sample size, overall p-values, type of variable ('categorical', 'normal' or 'non-normal') and the subset of individuals selected.

  'summary' returns a much more detailed list. Every component of the list is the result for each row-variable, showing frequencies, mean, standard deviations or quartiles as appropriate. Also, it shows overall p-values as well as p-trends and pairwise p-values among the groups.
 
  'plot' displays multiple devices with normality plots and Shapiro-Wilks test, for each of the continuous row-variables. If row-variable has less than 5 different values, it plots nothing.
  
An update method for 'compareGroups' objects has been implemented and works as usual to change all the arguments of previous analysis.

See examples for further illustration about all previous issues. 

}


\note{

By default, the labels of the variables (row-variables and grouping variable) are displayed in the resulting tables. These labels are taken from the "label" attribute of each variable. And if this attribute is NULL, then the name of the variable is displayed, instead. 
If a non-labelled variable is desired to be labelled, use the function \code{\link[Hmisc]{label}}.  \cr

Currently, 'plot' method only works under Windows platform, since it opens multiple \code{\link[grDevices]{x11}} devices. \cr

}

\seealso{
\code{\link{createTable}}, \code{\link{cGroupsGUI}}
}
\examples{

data(myData)

# by formula
ans<-compareGroups(y~.,data=myData)
ans
summary(ans)
update(ans,y~.-a)

# by data.frame
X<-myData[,c("a","b","c")]
y<-myData[,"y"]
ans<-compareGroups(X,y)
ans
summary(ans)

}

\keyword{misc}
