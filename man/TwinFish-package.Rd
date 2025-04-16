\name{TwinFish-package}
\alias{TwinFish-package}
\alias{TwinFish}
\docType{package}
\title{
\packageTitle{TwinFish}
}
\description{
\packageDescription{TwinFish}
}
\details{
This package only includes one R function which then subsequently calls a function in Rcpp. The R wrapper function allows you to set the number of years and simulations you want the model to run for as well as set all the parameters needed to run the Rcpp model itself. The model returns a list which inclues a number of different outputs, some of which are 3D arrays and some of which are lists themselves.

To run the code requires a C++ compiler, Rtools, Rcpp and RcppArmadillo. Windows machines have a native C++ compiler, Macs require downloading the Xcode and Xquartz apps. If the code won't run check the error to see if it is having problems with Rtools, in which case it may be that your Rtools is not actually downloaded into where your R packages normally sit and Rstudio can't find it, it which case you need to move Rtools to the right folder and that should fix that error. 
}
\author{
\packageAuthor{TwinFish}

Maintainer: \packageMaintainer{TwinFish}
}
\references{
Please see Aston et al. (in review) for relevant equations and references, as well as a full description of the model as applied to Spangled Emperor in the Ningaloo World Heritage Area (will update when published)
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
Help files for Rcpp, RcppArmadillo and Rtools.
}
\examples{
}
