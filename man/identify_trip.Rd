\name{identify_trip}
\alias{identify_trip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify fishing trips
}
\description{

Identifies complete trajectories of fishing trips from VMS data. It identify the fishing trips from the time the vessels leave port until they return. To do this, you need to generate the variables velocity and distance and a file with the coordinates of the landing harbors.

}
\usage{
identify_trip(data = data, dharbor = 2, vharbor = 5, rmin = 4, vmax = 20, hmax = 2.3, dmin = 5, see = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
is the data vms processed with the function processing_vms
}
  \item{dharbor}{
maximum distance from a registry to the port to be considered as a registry in port, it is expressed in hours.
}
  \item{vharbor}{
the maximum speed that a boat can reach when it is in the port, it is expressed in knots.
}
  \item{rmin}{
number of minimum vms records that contains the trajectory of a fishing trip.
}
  \item{vmax}{
maximum speed that a trip can reach during the fishing trip.
}
  \item{hmax}{
maximum hours that two vms records can contain}
  \item{dmin}{
minimum distance where a vessel is operating
}
  \item{see}{
%%     ~~Describe \code{see} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
