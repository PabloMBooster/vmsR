\name{calculate_course}
\alias{calculate_course}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Course of the vessel
}
\description{
\code{calculate_course} internally uses the \code{atan2} function to calculate the vessel's course (angle), then the \%\% operator is used to standardize the angle to 360 degrees. Then, the obtained angle is multiplied by 180/pi to convert the response in radians (the default output of atan2) to degrees.

Up to this point, the angle obtained is based on the standard trigonometric form where "East" = 0 degrees. So, it is necessary to scale the obtained angle to the navigation scale where "North" = 0. This is obtained by subtracting our obtained angle with the 90 degrees angle.
}
\usage{
calculate_course(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
longitude in decimal degrees
}
  \item{y}{
latitude in decimal degrees
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
