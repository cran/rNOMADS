\name{MagnitudeAzimuth}
\alias{MagnitudeAzimuth}
\title{
Convert zonal-meridional wind speeds to magnitude/azimuth.
}
\description{
Given zonal (East-West) and meridional (North-South) wind speeds, calculate magnitude of wind vector and azimuth from north, in degrees.
}
\usage{
MagnitudeAzimuth(zonal.wind, meridional.wind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zonal.wind}{
     A vector of zonal (East-West) winds, west negative.
}
  \item{meridional.wind}{
     A vector of meridional (North-South) winds, south negative.
}
}
\value{
\item{winds$magnitude}{Magnitude of wind vector.}
\item{winds$azimuth}{Azimuth of wind vector in degrees from North}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\examples{

zonal.wind <- c(35.5, -2)
meridional.wind <- c(-5, 15)
winds <- MagnitudeAzimuth(zonal.wind, meridional.wind)
print(winds$magnitude)
print(winds$azimuth)
}
\keyword{ manip }
