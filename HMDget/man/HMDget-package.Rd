\name{HMDget-package}
\alias{HMDget-package}
\alias{HMDget}
\docType{package}
\title{Get data from the Human Mortality Database into R}
\description{
Data can be accessed either directly from the web, using your username and password, or from the unpacked HMD_COUNTRIES database, which can be downloaded as a single .zip file from \link{www.mortality.org}. Has facilities for selecting particular years, variables and countries. If no arguments are specified, list menus pop up to handle all the essential arguments, and at the end the syntax necessary to replicate the menu choices is spit back to the console (just copy and paste it to repeat the same with no menus).}
\details{
\tabular{ll}{
Package: \tab HMDget\cr
Type: \tab Package\cr
Version: \tab 1.25\cr
Date: \tab 2011-07-11\cr
License: \tab General Public\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tim Riffe, with parts by Carl Boe

Maintainer: <tim.riffe@gmail.com>
}
\references{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de. 
}
\keyword{ package }
\keyword{ data }
\keyword{ manip }
\seealso{\code{\link[demography]{hmd.mx}}, \code{\link[MortalitySmooth]{selectHMDdata}}}
\examples{
library(HMDget)
myusername <- "myHMDname"
mypassword <- "myHMDpassword"
# these are the minimum arguements for web access, menus will pop up for everything else.
DATA <- HMDget(username=myusername,password=mypassword)
# when complete, the syntax necessary to repeat this with no menus should appear on the console

# if no arguments are specified at all:
DATA <- HMDget()
# then you can select local access, a folder selection menu comes up. Just navigate to your HMD_ COUNTRIES folder
# and select OK. This can also be specified via the \code{path} argument. 

# By default, data is put into R in a list format, with countries as the first level, 
# and selected variables as the second list level within countries. 
# try \code{format} = 1, 2 or 3 for other output formats.
}
