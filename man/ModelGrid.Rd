\name{ModelGrid}
\alias{ModelGrid}
\title{
Transform model data into an array
}
\description{
This function takes output from \code{\link{ReadGrib}} or \code{\link{DODSGrab}} and produces an array with dimensions: levels x variables x longitudes x latitudes.
This greatly reduces the size of the data set as well as makes it easier to manipulate.
The data must be in a regular latitude/longitude grid (like the GFS model, for example).
}
\usage{
ModelGrid(model.data, resolution, 
    levels = NULL, variables = NULL, 
    model.domain = NULL)
}
\arguments{
  \item{model.data}{
  Output from \code{\link{ReadGrib}}.
}
  \item{resolution}{
  Resolution of grid, in degrees if \code{grid.type = "latlon"}, in kilometers if \code{grid.type = "cartesian"}, as a 2 element vector \code{c(East-West, North-South)}.
}
  \item{levels}{
  The model levels to include in the grid, if NULL, include all of them.
}
  \item{variables}{
  The model variables to include in grid, if NULL, include all of them.
}
  \item{model.domain}{
  A vector c(LEFT LON, RIGHT LON, TOP LAT, BOTTOM LAT) of the region to include in output. 
  If NULL, include everything.
}
}
\details{
If you set the spacing of lon.grid and/or lat.grid coarser than the downloaded model grid, you can reduce the resolution of your model, possibly making it easier to handle.
}
\value{
\item{z}{An array of dimensions levels x variables x lon x lat; each level x variable contains the model grid of data from that variable and level}
\item{x}{Vector of longitudes}
\item{y}{Vector of latitudes}
\item{variables}{The variables contained in the grid}
\item{levels}{The levels contained in the grid}
\item{model.run.date}{When the forecast model was run}
\item{fcst.date}{The date of the forecast}
}

\note{
Only use this function when the model grid is regular.
For example, the GFS high resolution model is 0.5 x 0.5 degree across its domain.
I have provided this function as a convenience since I only use it for manipulating GFS model data.
I am not sure how well it works for other models. 
Consider yourself warned!
}

\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\seealso{
\code{\link{ReadGrib}}, \code{\link{DODSGrab}}
}
\examples{
\dontrun{
#Get some example data 
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)
model.parameters <- ParseModelPage(urls.out[1])
levels <- c("2 m above ground", "100 mb")
variables <- c("TMP", "RH") #Temperature and relative humidity
grib.info <- GribGrab(urls.out[1], model.parameters$pred[1], levels, variables)
#Extract the data
model.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)

#Make it into an array
gfs.array <- ModelGrid(model.data, c(0.5, 0.5))

#What variables and levels we have
print(gfs.array$levels)
print(gfs.array$variables)

#Find minimum temperature at the ground surface, and where it is
min.temp <- min(gfs.array$z[2, 1,,] - 273.15)
sprintf("\%.1f", min.temp) #in Celsius 

ti <- which(gfs.array$z[2, 1,,] == min.temp + 273.15, arr.ind = TRUE)

lat <- gfs.array$y[ti[1,2]] #Lat of minimum temp
lon <- gfs.array$x[ti[1,1]] #Lon of minimum temp

#Find maximum temperature at 100 mb atmospheric pressure
max.temp <- max(gfs.array$z[1, 1,,]) - 273.15
sprintf("\%.1f", max.temp) #Brrr!
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
