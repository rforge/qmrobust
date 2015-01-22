\name{plot.interlab}
\alias{plot.interlab}

\title{
Plot an interlab object
}
\description{
Plots the data and statistics of an interlaboratory study
}
\usage{
\method{plot}{interlab}(x, data=NULL, sd = TRUE, difflimits = TRUE,
     grouplabels = NULL, cex.text = 1.1, col = "blue", ...)
}
\arguments{
  \item{x}{
    an object of S3-class \code{interlab}
  }
  \item{data}{
    Data set containing the variables that appear in
    \code{x$call$formula}.
    Not needed if \code{object} contains the data.}
  \item{sd}{
    logical: should the standard deviations (within and betweengroups)
    be plotted?}
  \item{difflimits}{
    logical: should the standard deviations (within and betweengroups)
    be plotted?}
  \item{ylim}{Limits for the y axis.}
  \item{grouplabels}{
    Labels for the groups. Defaults to code{substring(lev,1,3)}, where
    \code{lev} are the levels of the group factor}
  \item{cex.text}{Size of the text labelling the standard deviations}
  \item{col}{Color to be used for the group means and the respective
    standard deviation}
  \item{\dots}{{}Further arguments passed to \code{plot.default}}
}
\value{none; graphics routine}
\references{
%% ~put references to the literature/web site here ~
}
\author{{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{interlabstats}}}
\examples{
data(d.perm)
dd <- d.perm[d.perm$section=="w.1",]
dd[dd$team=="E","perm.log"] <- dd[dd$team=="E","perm.log"] - 0.3
  ## otherwise, there is no between group variation
r.cl <- interlabstats(perm.log~team, data=dd)
plot(r.cl) 
}
\keyword{hplot}
