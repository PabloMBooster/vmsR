\name{training_nnet}
\alias{training_nnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Training of the neural network}
\description{
Calibration a artificial neural network to detected fishing sets in vms data.
The following variables are required for network training: hour, velocity, change of velocity 1,
change of velocity 2 and change of course.

The neural network is trained on the basis of the next job (joo 2011). For the validation of the model check the percentage of successes and for increase the precision of the model it is necessary to increase the number of loops.

We use the nnet package and any argument can be modified to improve the performance.

}
\usage{
training_nnet(data, directory, neurons = 4, loops = 10, thres_min = 0.4, thres_max = 0.6, MSE_max = 0.04, prop_train = 0.75, T1 = 180, T2 = 360)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data sets
}
  \item{directory}{
the working directory}
  \item{neurons}{
numbers of neurons
}
  \item{nb_loop}{
numbers of loops
}
  \item{thres_min}{
minimun threshold
}
  \item{thres_max}{
maximun threshold
}
  \item{MSE_max}{
%%     ~~Describe \code{MSE_max} here~~
}
  \item{prop_train}{
%%     ~~Describe \code{prop_train} here~~
}
  \item{T1}{
%%     ~~Describe \code{T1} here~~
}
  \item{T2}{
%%     ~~Describe \code{T2} here~~
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
- Joo R., Bertrand Sophie, Chaigneau Alexis, Niquen M. (2011). Optimization of an artificial neural network for identifying fishing set positions from VMS data : an example from the Peruvian anchovy purse seine fishery. Ecological Modelling, 222 (4), 1048-1059. ISSN 0304-3800.
\url{http://www.documentation.ird.fr/hor/fdi:010053066}.

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

head(data_vms)

loops = 2
neurons = 4
MSE_max = 0.04

# calibration
nnet_out <- training_nnet(data = data_vms, directory = getwd(), neurons = neurons, MSE_max = MSE_max, loops = loops)

# prediction
data_vms$Calas <- predict_sets(data = data_vms, directory = getwd(), loops = loops)

# map
require(maps)
plot(data_vms$Lon, y = data_vms$Lat, cex = 0.4, pch = 16, xlab = "lon", ylab = "lat")
points(data_vms$Lon[data_vms$Calas == 1], y = data_vms$Lat[data_vms$Calas == 1], col = 2, cex = 0.4)
map("worldHires",fill=T, myborder = FALSE, add = TRUE, col = "khaki1")
box()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
