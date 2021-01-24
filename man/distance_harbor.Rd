\name{distance_harbor}
\alias{distance_harbor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance harbor
}
\description{
Calculates the distance between a vessel's location and the nearest port. It is necessary to have a file of the locations of all landing ports in the fishery. The distance calculated is the orthodromic distance and expressed in nautical miles. The function returns the distance and the name of the port and is used internally by the function \code{processing_vms}.
}
\usage{
distance_harbor(data_vms, harbor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_vms}{
vms data
}
  \item{harbor}{
archive of landing port locations for the peruvian industrial purse seine fishery
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
