\name{processing_vms}
\alias{processing_vms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Processing VMS data
%%  ~~function to do ... ~~
}
\description{
Processing vms data and calculates variables about distance, time, velocity, change velocity and course.
}
\usage{
processing_vms(data, vessel = "num_vessel", harbor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data raw of the vessel monitoring system
}
  \item{vessel}{

Identifier of the vessel and must be in character format
}
  \item{harbor}{
The file with the coordinates of the harbor
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
