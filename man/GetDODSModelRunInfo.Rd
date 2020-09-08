\name{GetDODSModelRunInfo}
\alias{GetDODSModelRunInfo}
\title{
Get model coverage and data information for models on GrADS-DODS system.
}
\description{
Given a URL from \code{\link{GetDODSDates}} and a model run from \code{\link{GetDODSModelRuns}}, get information on the model domain, levels, and variables.
}
\usage{
GetDODSModelRunInfo(model.url, model.run, download.file=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.url}{
  A URL for a model on the GrADS - DODS system, probably returned by \code{\link{GetDODSDates}}.
}
  \item{model.run}{
   A specific model run, probably returned by \code{\link{GetDODSModelRuns}}
}
  \item{download.file}{
     If \code{TRUE}, download the DODS \code{.info} file and read it in.
     If \code{FALSE}, try and read the \code{.info} file directly from the Internet.}
}
\details{
This routine grabs information about the latitude, longitude, and time coverage of a specific model instance.
It also finds data about levels (if present) and lists all the available variables (though they may not have data in them).
The user can refer to this information to construct calls to the DODS system via \code{\link{DODSGrab}}.
Note that the \code{XML} package has trouble interpreting https from time to time; this can be circumvented by keeping \code{download.file=TRUE}.
}
\value{
\item{model.info}{Information provided by the GrADS - DODS system about the given model instance.}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
This function is very helpful in figuring out what's inside poorly documented models.
}
\seealso{
\code{\link{GetDODSDates}}, \code{\link{GetDODSModelRuns}}, \code{\link{DODSGrab}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get some information about the latest model url and date, real time server
abbrev <- "gfs_0p50"
\dontrun{
urls.out <- GetDODSDates(abbrev)
model.url <- tail(urls.out$url, 1)
model.runs <- GetDODSModelRuns(model.url)
model.info <- GetDODSModelRunInfo(model.url, tail(model.runs$model.run, 1))
print(model.info)
}
}
\keyword{ utilities }
