\name{NOMADSRealTimeList}
\alias{NOMADSRealTimeList}
\title{
Models available for download through rNOMADS
}
\description{
A list of abbreviations, names and URLs for the NOMADS models.
Users can refer to this list to find out more information about the available models, and rNOMADS uses the abbreviations to determine which URLs to scan and download.
}
\usage{
NOMADSRealTimeList(abbrev = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abbrev}{
   Return information about the model that this abbreviation refers to.
   Defaults to \code{NULL}, in which case information about all the models available through \code{rNOMADS}.
}
}
\value{
    \item{abbrevs}{An abbreviation for each model}
    \item{names}{A full name for each model}
    \item{urls}{The web address of the download page for each model}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{WebCrawler}}, \code{\link{ParseModelPage}}, \code{\link{NOMADSArchiveList}}, \code{\link{GribGrab}}
}
\examples{

#The full model list in rNOMADS

model.list <- NOMADSRealTimeList()

}
\keyword{ documentation }
