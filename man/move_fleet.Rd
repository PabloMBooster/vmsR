\name{move_fleet}
\alias{move_fleet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
move fleet
}
\description{
Generates a fleet movement map in giff format. Trip trajectory data must be interpolated at a constant time step.
}
\usage{
move_fleet(data_inte = data_inte, xlim = c(-86, -70), lwdArrows = 1, ylim = c(-21, -3), interval = 0.9, movie.name = "movie.gif", nmax = 50, file_harbor = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_inte}{
interpolated trip trajectory data
}
  \item{xlim, ylim}{
numeric vectors of length 2, giving the x and y coordinates ranges}
  \item{lwdArrows}{
arrow width
}
  \item{interval}{
animation time, default is 0.8 seconds
}
  \item{movie.name}{
file name of the movie
}
  \item{nmax}{
maximum number of images, default is 50
}
  \item{file_harbor}{
file containing latitude and longitude of the ports
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
Pablo Marin
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
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
