\name{HMDget}
\alias{HMDget}

\title{Get data from the Human Mortality Database into R}
\description{Data can be accessed either directly from the web, using your username and password, or from the unpacked HMD_COUNTRIES database, which can be downloaded as a single .zip file from \link{www.mortality.org}. Has facilities for selecting particular years, variables and countries. If no arguments are specified, list menus pop up to handle all the essential arguments, and at the end the syntax necessary to replicate the menu choices is spit back to the console (just copy and paste it to repeat the same with no menus).}
\usage{
HMDget(countries = NULL, wanteditems = NULL, years = TRUE, drop.tadj = TRUE, format = 0, path = NULL, username, password, syntax = TRUE)
}

\arguments{
  \item{countries}{
A character vector of HMD country codes. If left unspecified, a menu will pop up, and you can select one or more countries to extract. The pop up list will contain the HMD country abbreviations, not the full country names.
}
  \item{wanteditems}{
A character vector of the items you want extracted, e.g. \code{"Mx_1x1"}. If left unspecified, a menu will pop up, and you can select one or more items to extract. You will have the option to display items available in ANY of you selected countries or ALL of your selected countries. Usualy it makes sense to limit the list to those variables in all of your selected countries.
}
  \item{years}{
A list of character or numeric vectors specifying the years to be extracted for each variable. If multiple countries are selected, their years will be limited to those specified in the \code{years} argument. A separate years vector is needed for each item because different items may have different ways of indicating years, since some are year ranges and others are precise years. The \code{years} list should contain an element for each item you are extracting, named accordingly, e.g. \code{years\$Mx_1x1} should be a numeric vector, e.g. \code{c(1980:2007)}. If this argument is not specified, then it is handled with menus. A separate menu will pop up for each item, first asking whether you want to years available in any country or in all countries. It is safer to select years available in all countries if you want formats 1,2 or 3. You can see an example of how to properly specify it in the syntax return.
}
  \item{drop.tadj}{
logical, default = \code{TRUE}. Some years in some countries have a "+" or "-" appended to the year to indicate upper and lower population estimates owing to changes in a country's territory in a given year. If TURE, we keep only the "+" variant. This option by Carlo Boe.
}
  \item{format}{
=c(0,1,2,3,4). There are 4 data extract formats available, to save users some time rearranging data. The default format is 0. Output will be a list of countries, where each country is a list of items, and each item is a \code{data.frame} containing all columns of data given by the HMD, e.g. Male, Female, Total. These are in the default HMD long format, with years stacked in rows, and ages nested within years. If \code{format = 1}, HMDget tries to coerce all output into a single \code{data.frame}. This will only work if the data dimensions permit for each country and item, meaning that the years must be specified equally for each item, and all of the selected years must be available for each of the selected counties. In this case, the big output \code{data.frame} will have a column for each country-item-column, e.g. \code{USA.MX_1x1.Male}. Redundant Year and Age columns are removed, but all other data columns are preserved in this \code{data.frame}, so the user may still have some pruning to do if not all columns are desired. If \code{format = 2}, output will be a list of items, where each item is a \code{data.frame} in long format with a column for each country-column combo for that item. Again, for this option different countries can only be stuck together into a \code{data.frame} if the selected years were available for all selected countries (this is made easy with menu year selection). For formats (0,1,2) , if only 1 country and 1 item are selected, output will be as a \code{data.frame} and not as the default list. If \code{format = 3} output will consist in a list of items, where each item is a list of the data columns given for that item, e.g. Male, Female, Total, and each of these data columns is reshaped into a \code{dataframe}, with years in columns and countries stacked in rows, ages nested within countries. \code{format = 4} is intended for selections of 1 country, 1 item, and only 1 column (not including Age and Year) from that item (multiple years OK). The output for format = 4 will be a numeric matrix with years in columns (names) and ages in rows (names).
}
  \item{path}{
a character string specifying the file path to the folder containing the HMD country folders, if local access is preferred. If this has been downloaded (from http://www.mortality.org/hmd/zip/all_hmd/hmd_countries.zip) , unpacked and unaltered, the folder is called "HMD_COUNTRIES". If you are navigating to this folder in the selection menus, then only go as far as the HMD mother folder and click OK. Local access is faster, but web access always guarantees and up-to-date database. 
}
  \item{username}{
a character string specifying your HMD username. Only required if web access is desired. This is not sent encrypted.
}
  \item{password}{
a character string specifying your HMD password. Only required if web access is desired. This is not sent encrypted.
}
  \item{syntax}{
logical. Default = \code{TRUE}. Should the function spit back syntax to the console? If you used menus, or are just getting started with the function, then this is helpful for exact repeating, or for slight modifications.
}
}
\details{
In general, local access is recommended because data are extracted faster. Web access always guarantees that the database is fully up-to-date. Be aware that if you use web access, your username and password are not sent with encryption. The code used for web access is based on code written by Carlo Boe in his unpackaged function \code{HMD2R}, available at the Applied Demographer's Toolbox (http://www.demog.berkeley.edu/~eddieh/toolbox.html\#HMD2R). The web access depends on the RCurl package, which you may need to install prior to using this function.
}

\value{
your HMD data extract in the format specified. It will either be a \code{list} or a \code{data.frame}. See above argument \code{format} for details.}
\references{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de.
}
\author{
Tim Riffe, with web access and some options by Carl Boe.
}
\note{
Please report errors. I'll fix them! I'll also consider implementing other output formats by request.
}
\seealso{\code{\link[demography]{hmd.mx}}, \code{\link[MortalitySmooth]{selectHMDdata}}}
\examples{
library(HMDget)
myusername <- "myHMDname"
mypassword <- "myHMDpassword"
# these are the minimum arguements for web access, menus will pop up for everything else.
DATA <- HMDget(username=myusername,password=mypassword)

# when complete, the syntax necessary to repeat this with no menus should appear on the console
names(DATA)
names(DATA[[1]])
head(DATA[[1]][[1]])
dim(DATA[[1]][[1]])

# put a particular data column into a different format:
THISITEM <- DATA[[1]][[1]]
Years <- unique(THISITEM[,1])
Ages <- unique(THISITEM[,2])

# assuming the item has a Male column, we can put years in the columns like this:
MALESTIME <- matrix(THISITEM[,4],nrow=length(Ages),ncol=length(Years))
colnames(MALESTIME) <- Years ; rownames(MALESTIME) <- Ages
head(MALESTIME)
plot(MALESTIME[,1])
image(MALESTIME)
image(log(MALESTIME),col=rev(heat.colors(50)))


# if no arguments are specified at all:
DATA <- HMDget()
# then you can select local access, a folder selection menu comes up. Just navigate to your HMD\_ COUNTRIES folder
# and select OK. This can also be specified via the \code{path} argument. Or you can select web access, 
# in which case the function will stop and ask you to specify your username and password.

# By default, data is put into R in a list format, with countries as the first level, 
# and selected variables as the second list level within countries. 
# try \code{format} = 1, 2 or 3 for other output formats.
}

\keyword{ data }
\keyword{ manip }