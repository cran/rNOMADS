\name{ReadGrib}
\alias{ReadGrib}
\title{
Extract data from grib files
}
\description{
This function wraps \code{wgrib2} and \code{wgrib}, external grib file readers provided by the National Weather Service Climate Prediction Center (see \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} and \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}).
\code{ReadGrib} extracts forecast data into R.
It does this by building an argument string, executing a system call to the appropriate external grib file reader, and extracting the result.
Note that \code{wgrib2} must be installed for \code{ReadGrib} to work for current grib files, and \code{wgrib} may need to be installed when looking at archived data.
}
\usage{
ReadGrib(file.names, levels, variables, 
    forecasts = NULL, domain = NULL, domain.type = "latlon", 
file.type = "grib2", missing.data = NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.names}{
The path and file name of the grib files to read.
}
  \item{levels}{
  The levels to extract.
}
  \item{variables}{
  The variables to extract.
}
  \item{forecasts}{
  Names of forecasts to extract.  
  If \code{NULL}, include everything.
}
  \item{domain}{
  Include model nodes in the specified region: \code{c(LEFT LON, RIGHT LON, NORTH LAT, SOUTH LAT)}.
  If \code{NULL}, include everything.
  This argument works for GRIB2 only.
}
  \item{domain.type}{
  Either \code{\"latlon\"} 
  (the \code{domain} argument is a latitude/longitude quadruplet) 
  or \code{\"index\"} (the \code{domain} argument is a model node index quadruplet; 
  much faster but requires you to know the model setup)
  }
  \item{file.type}{
  Whether the file is in GRIB (\code{"grib1"}) or GRIB2 (\code{"grib2"}) format.
  Default is \code{"grib2"}.
}
  \item{missing.data}{
  Replace missing data in grib archive with this value.
  If \code{NULL}, leave the data out.  Only works with wgrib2. Default \code{NULL}.
}
}
\details{
This function constructs system calls to \code{wgrib} and \code{wgrib2}.
Therefore, you must have installed these programs and made it available on the system path.
Unless you are interested in accessing archive data that's more than a few years old, you can \code{install} wgrib2 only.
A description of \code{wgrib2} and installation links are available at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} and \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}.
Also, rNOMADS is focused towards GRIB2 files; I have included GRIB1 format support as a convenience.
}
\value{
\item{model.data}{A structure with a series of elements containing data extracted from the grib files.}
}
\references{
Ebisuzaki, W, Bokhorst, R., Hyvatti, J., Jovic, D., Nilssen, K,  
    Pfeiffer, K.,  Romero, P.,  Schwarb, M., da Silva, A., Sondell, N., and Varlamov, S. (2011).
    wgrib2: read and write GRIB2 files.  \emph{National Weather Service Climate Prediction Center},
    \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\seealso{
\code{\link{GribGrab}}, \code{\link{ArchiveGribGrab}}, \code{\link{ModelGrid}}
}
\examples{

#Operational Forecast Data Extraction
#NCEP output is always in GRIB2 format - this makes things easy for us
#An example for the Global Forecast System 0.5 degree model

#Get the latest model url
\dontrun{
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)

#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[1])

#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date) 

my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#Get the data
model.info <- GribGrab(urls.out[1], my.pred, levels, variables)

#Extract the data
model.data <- ReadGrib(model.info[[1]]$file.name, levels, variables)

#Reformat it
model.grid <- ModelGrid(model.data, c(0.5, 0.5))

#Show an image of world temperature at ground level
image(model.grid$z[2, 1,,])

}
}
\keyword{ file }
