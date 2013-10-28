\name{ModelGrid}
\alias{ModelGrid}
\title{
Transform model data into an array
}
\description{
This function takes output from \code{\link{ReadGrib}} and produces an array with dimensions: levels x variables x longitudes x latitudes.
This greatly reduces the size of the data set as well as makes it easier to manipulate.
However, it only works for some models (see notes section).
}
\usage{
ModelGrid(model.data, levels = NULL, variables = NULL, model.domain = NULL)
}
\arguments{
  \item{model.data}{
  Output from \code{\link{ReadGrib}}.
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
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}
\note{
The purpose of \code{rNOMADS} is to provide an interface to, and data extraction from, the NOMADS models.
In general, the manipulation of the returned data is left up to the user.
However, I did include \code{ModelGrid} in the hopes that it might be useful (or at least a starting point) for data manipulation.
This function should work well for regularly-spaced lat/lon grids (such as the Global Forecast System).
It will definitely not work for the North American Mesoscale models, as they are on a 12 kilometer grid (and thus the lat/lon spacing changes over the model).}

\seealso{
\code{\link{ReadGrib}}
}
\examples{

#Get some example data 
#Global Forecast Model 0.5x0.5 resolution - outputted by ReadGrib)
#This data is from late July and covers the state of North Carolina and nearby regions
data(GFS)

#Make it into an array
gfs.array <- ModelGrid(gfs.model.data)

#What variables and levels we have
print(gfs.array$levels)
print(gfs.array$variables)

#Find minimum temperature at the ground surface, and where it is
min.temp <- min(gfs.array$z[2, 1,,] - 273.15)
sprintf("\%.1f", min.temp) #in Celsius 

ti <- which(gfs.array$z[2, 1,,] == min.temp + 273.15, arr.ind = TRUE)

#Turns out two locations get the minimum temperature prize.
#One of them is near Cumberland, Kentucky.
lat <- gfs.array$y[ti[1,2]]
lon <- gfs.array$x[ti[1,1]]

#Find maximum relative humidity at ground level
max.rh <- max(gfs.array$z[2, 2,,])
sprintf("\%.1f", max.rh) 

#Find maximum temperature at 100 mb atmospheric pressure
max.temp <- max(gfs.array$z[1, 1,,]) - 273.15
sprintf("\%.1f", max.temp) #Brrr!

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
