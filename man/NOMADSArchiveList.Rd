\name{NOMADSArchiveList}
\alias{NOMADSArchiveList}
\title{
Archived models available for download through rNOMADS
}
\description{
A list of abbreviations, names and URLs for the NOMADS models archived on the NCDC web site..
Users can refer to this list to find out more information about the available models, and rNOMADS uses the abbreviations to determine how to access the archives.
}
\usage{
NOMADSArchiveList(abbrev = NULL)
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
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\seealso{
\code{\link{NOMADSRealTimeList}}
}
\examples{

#The archived model list in rNOMADS

archived.model.list <- NOMADSArchiveList()

}
\keyword{ documentation }
