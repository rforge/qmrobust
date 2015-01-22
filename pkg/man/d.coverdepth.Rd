\name{d.coverdepth}
\alias{d.coverdepth}
\alias{d.coverdepthgrid}
\docType{data}
\title{
Cover depth data
}
\description{
Concrete cover depth of the reinforcement layer of the deck slab on a
freeway viaduct, measured for a \eqn{latex}{5\times 40} grid.
}
\usage{data("d.coverdepth")}
\format{
  \code{d.coverdepthgrid} is a matrix containing the depth values
  for the \eqn{latex}{5\times 40} gridpoints.
  
  \code{d.coverdepth} is a  data frame containing the same 200 depth
  readings, as given by the following 4 variables. 
  \describe{
    \item{\code{depth}}{Depth [mm]}
    \item{\code{row}}{Position in the width of the bridge}
    \item{\code{column}}{Position along the bridge}
    \item{\code{section}}{The bridge is divided into 5 sections}
  }
}
%%- \details{
%%- %%  ~~ If necessary, more details than the __description__ above ~~
%%- }
\source{
  Monteiro, A. V., Gon\c calves, A. and Gulikers, J. (2014).
  Statistical bases for assessing the cover depth in reinforced concrete
  structures.
  Performance-Based Specifications and Control of Concrete Durability,
  RILEM TC 230-PSC, RILEM, Fig. 9.4.
}
\references{
  Stahel, W.A., Moro F. and Luco, L.F. (2015).
  ``Statistical Procedures for Performance-based Specification and
  Testing''.
  In: 
}
\examples{
data(d.coverdepth)
showd(d.coverdepth)

dd <- d.coverdepth[d.coverdepth$section<=2,]
symbols(dd$column,dd$row, circles=dd$depth, inches=par("cin")[1],
        xlab="",ylab="")
}
\keyword{datasets}
