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
identify_trip(data = data, dharbor = 2, rmin = 6, vmax = 16, vmin = 16, hmax = 2.3,)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
VMS dataset.
}
  \item{dharbor}{
distance of the vessel from the harbor when the vessel is still in harbor, it is expressed in nautical miles.
}
  \item{rmin}{
number of minimum vms records that contains the trajectory of a fishing trip.
}
  \item{vmax}{
maximum velocity that a vessel can reach during the fishing trip.
}
  \item{vmin}{
threshold of velocity to consider the vessel as having made a fishing trip
}
  \item{hmax}{
maximum time between two contiguous vms records
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
