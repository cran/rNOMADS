\name{ReadGrib}
\alias{ReadGrib}
\title{
Extract data from grib file
}
\description{
This function wraps \code{wgrib2} and \code{wgrib}, external grib file readers provided by the National Weather Service Climate Prediction Center (see \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} and \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}).
\code{ReadGrib} extracts forecast data into R.
It does this by building an argument string, executing a system call to the appropriate external grib file reader, and extracting the result.
Note that \code{wgrib2} must be installed for \code{ReadGrib} to work for current grib files, and \code{wgrib} may need to be installed when looking at archived data.
}
\usage{
ReadGrib(file.name, levels, variables, file.type = "grib2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
The path and file name of the grib file to read.
}
  \item{levels}{
  The levels to extract.
}
  \item{variables}{
  The variables to extract.
}
  \item{file.type}{
  Whether the file is in GRIB (\code{"grib1"}) or GRIB2 (\code{"grib2"}) format.
  Default is \code{"grib2"}.
}
}
\details{
This function constructs system calls to \code{wgrib} and \code{wgrib2}.
Therefore, you must have installed these programs and made it available on the system path.
Unless you are interested in accessing archive data that's more than a few years old, you can \code{install} wgrib2 only.
A description of \code{wgrib2} and installation links are available at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} and \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}.
Note that Windows users will likely have to use a UNIX emulator like Cygwin to install these programs.
Also, rNOMADS is focused towards GRIB2 files; I have included GRIB1 format support as a convenience.
}
\value{
\item{model.data}{A structure with a series of elements containing data extracted from the grib file.}
}
\references{
Ebisuzaki, W, Bokhorst, R., Hyvatti, J., Jovic, D., Nilssen, K,  
    Pfeiffer, K.,  Romero, P.,  Schwarb, M., da Silva, A., Sondell, N., and Varlamov, S. (2011).
    wgrib2: read and write GRIB2 files.  \emph{National Weather Service Climate Prediction Center},
    \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{GribGrab}}, \code{\link{ArchiveGribGrab}}, \code{\link{ModelGrid}}
}
\examples{

#Operational Forecast Data Extraction
#NCEP output is always in GRIB2 format - this makes things easy for us
#An example for the Global Forecast System 0.5 degree model

#Get the latest model url
\dontrun{urls.out <- CrawlModels(abbrev = "gfs0.5", depth = 1)}

#Get a list of forecasts, variables and levels
\dontrun{model.parameters <- ParseModelPage(urls.out[1])}

#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date) 

\dontrun{my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]}

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#Get the data
\dontrun{model.info <- GribGrab(urls.out[1], my.pred, levels, variables)}

#Extract the data
\dontrun{model.data <- ReadGrib(model.info$file.name, levels, variables)}

#Reformat it
\dontrun{model.grid <- ModelGrid(model.data, c(0.5, 0.5))}

#Show an image of world temperature at ground level
\dontrun{image(model.grid$z[2, 1,,])}


#Archived Data Extraction
#This is sometimes in GRIB1 format
#This example is in GRIB1

abbrev <- "gfs-anl"
model.date <- 20040302 #March 2, 2004
model.run <- 18 #1800 GMT model run
pred <- 0 #Analysis

\dontrun{grib.info <- ArchiveGribGrab(abbrev, model.date, model.run,
    pred, file.type = "grib1")}

\dontrun{model.data <- ReadGrib(grib.info$file.name, c("1000 mb"), c("TMP"), 
    file.type = "grib1")}
}
\keyword{ file }
