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
ReadGrib(file.name, variables, levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
The path and file name of the grib file to read.
}
  \item{variables}{
  The variables to extract.
}
  \item{levels}{
  The levels to extract.
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

#Get the latest model date
model.date <- Sys.time()

#Get the forecast 6 hours from now - addition is defined in seconds
fcst.date <- model.date + 6 * 3600

#What region of the atmosphere to get data for
#2 meters above the ground and at 800 mb pressure
levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#What region of the world to get data for - omit this and you get the whole planet!
model.domain <- c(-84, -74, 37, 32) #Get the area around North Carolina, USA

file.name <- "fcst.grb" #Name of file to save downloaded data to

#Get the data
GribGrab(levels, variables, model.date = model.date,
    fcst.date = fcst.date, model.domain = model.domain, file.name = file.name)

#Read the data, only capturing temperature at the 2 m above ground level
\dontrun{model.data <- ReadGrib(file.name, c("TMP"), c("2 m above ground"))}

#Make it into an array
\dontrun{model.array <- ModelGrid(model.data, 0.5, 0.5)}

#Make a contour plot of the temperature around North Carolina, USA:
\dontrun{contour(x = model.array$x, y = model.array$y, model.array$z[1,1,,] - 273.15, 
    xlab = "Latitude", ylab = "Longitude", 
   main = paste("North Carolina Temperatures for", 
   model.array$fcst.date, "GMT in Celsius"))}


}
\keyword{ interface }
\keyword{ file }
