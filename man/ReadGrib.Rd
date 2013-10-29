\name{ReadGrib}
\alias{ReadGrib}
\title{
Extract data from grib file
}
\description{
This function wraps \code{wgrib2}, an external grib file reader provided by the National Weather Service Climate Prediction Center (see \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}).
\code{ReadGrib} extracts forecast data for the specified variables and levels into R in CSV format.
It does this by building an argument string, executing a system call to \code{wgrib2}, and extracting the result.
Note that \code{wgrib2} must be installed for \code{ReadGrib} to work.
}
\usage{
ReadGrib(file.name, levels, variables)
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
}
\details{
This function constructs a system call to \code{wgrib2}.
Therefore, you must have installed \code{wgrib2} and made it available on the system path.
A description of \code{wgrib2} and installation links are available at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}.
Note that Windows users will likely have to use a UNIX emulator like Cygwin to install \code{wgrib2}. 
}
\value{
An array containing \code{wgrib2} output, with headers:
\item{model.run.date}{When the model was computed (GMT)}
\item{forecast.date}{The forecast date (GMT)}
\item{variable}{The data type}
\item{level}{What vertical level the data is located in}
\item{lon}{Longitude value of grid point}
\item{lat}{The latitude value of grid point}
\item{value}{The value of the given variable, at the given level, at the location defined by \code{lon} and \code{lat}}
}
\references{
Ebisuzaki, W, Bokhorst, R., Hyvatti, J., Jovic, D., Nilssen, K,  
    Pfeiffer, K.,  Romero, P.,  Schwarb, M., da Silva, A., Sondell, N., and Varlamov, S. (2011).
    wgrib2: read and write GRIB2 files.  \emph{National Weather Service Climate Prediction Center},
    \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}
}
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}
\note{
The \code{wgrib2} utility is a very powerful grib file manipulator - the \code{rNOMADS} package only scratches the surface of what it can do.
}

\seealso{
\code{\link{GribGrab}}, \code{\link{ModelGrid}}
}
\examples{

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
\dontrun{file.name <- GribGrab(urls.out[1], my.pred, levels, variables)}

#Extract the data
\dontrun{model.data <- ReadGrib(file.name, levels, variables)}

#Reformat it
\dontrun{model.grid <- ModelGrid(model.data)}

#Show an image of world temperature at ground level
\dontrun{image(model.grid$z[2, 1,,])}

}
\keyword{ interface }
\keyword{ file }
