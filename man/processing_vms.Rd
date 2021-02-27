\name{processing_vms}
\alias{processing_vms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Processing VMS data
%%  ~~function to do ... ~~
}
\description{
The \code{processing_vms} automatically pre-processes raw VMS data. The VMS data must contain a vessel identifier variable (Cod_vessel or Name_vessel), a variable in date format (year, month, day and time), and the position variables (latitude and longitude) in decimal degrees.

The processing of the VMS data consists of: sorting the VMS data by vessel and date; calculate the distance between each VMS emission (orthodromic distance); calculate the distance from each emission to the nearest port; calculate the speed of the vessel at each time step; calculate the elapsed time in hours at each time instant; calculate course and change of course.

Distance variables are expressed in nautical miles, vessel speed in miles/hour, course and course change in decimal degrees.


}
\usage{
processing_vms(data, vessel = "num_vessel", harbor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
raw VMS data
}
  \item{vessel}{

variable to identify the vessel, it can be in character or numeric format.
}
  \item{harbor}{
data the coordinates of harbors
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
}
