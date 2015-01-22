\name{d.tunnel}
\alias{d.tunnel1}
\alias{d.tunnel2}
\docType{data}
\title{
Properties of concrete in a tunnel
}
\description{
  Samples of concrete used in the construction of an opencast tunnel are
  obtained and several properties are measured. Two boreholes of
  different depths are taken at the same 20 positions.
  This leads to two datasets.
}
\usage{data("d.tunnel1"); data("d.tunnel2")}
\format{
  Two data frames with 240 and 412 observations, respectively,
  on the following variables.
  \describe{
    \item{\code{section}}{Section of the tunnel profile,
      with levels \code{d.1} and \code{d.2} for decks 1 and 2, and
      \code{w.1} and \code{w.2} for walls 1 and 2}
    \item{\code{diameter}}{Diameter of bore hole, 68mm for
      \code{d.tunnel1} and 50mm for \code{d.tunnel2}}
    \item{\code{n}}{Number of ...???}
    \item{\code{position}}{Position, factor with levels \code{A} to \code{V}}
    \item{\code{layer}}{Layer, factor with levels
      \code{layer.1}: ...,
      \code{layer.2}: ...,
      \code{layer.3}: ...,
      \code{d.tunnel1} only contains the first 3 layers}
    \item{\code{strength}}{Compressive strength of the concrete [MPa]}
    \item{\code{density}}{Density []}
    \item{\code{perm.O2}}{Permeability for oxigen ???[m2]
      (only \code{d.tunnel1})}
    \item{\code{cond.CL}}{???
      (only \code{d.tunnel1})}
    \item{\code{pores.total}}{Pores [m2]
      (only \code{d.tunnel2})}
    \item{\code{pores.air}}{Pores for air [m2]
      (only \code{d.tunnel2})}
    \item{\code{pores.H2O}}{Pores for water [m2]
      (only \code{d.tunnel2})}
    \item{\code{water}}{???
      (only \code{d.tunnel2})}
    \item{\code{cond.H2O}}{???
      (only \code{d.tunnel2})}
  }
}
\details{
The samples for the two datasets are bore kernels of 68 and 50 mm
diameter, respectively. The samples with the same
\code{position} and \code{layer} are taken near each other. 
}
\source{
  Leemann, A., Hoffmann, C., Malioka, V. and Faber, M. (2006).
  Variability of concrete properties in structures,
  Technical Report 611, Bundesamt f\:ur Strassen (ASTRA), Bern, Switzerland.
}
\references{
  Stahel, W.A., Moro F. and Luco, L.F. (2015).
  ``Statistical Procedures for Performance-based Specification and
  Testing''.
  In: 
}
\examples{
data(d.tunnel1)
showd(d.tunnel1)
data(d.tunnel2)
showd(d.tunnel2)

## merge
d.tunnel <- merge(d.tunnel1, d.tunnel2,
  by=c("section","position","layer"), all.x=TRUE,
suffixes=c(".68",".50"))

## compare densities in neighboring boreholes
plot(density.50~density.68,d.tunnel)

## regression model to explain  permeability
r.lm <- lm(log10(pmax(1e-11,perm.O2))~log10(pmax(cond.CL,0.5))+
             density.68+section,  data=d.tunnel)
summary(r.lm)
plot(r.lm)
}
\keyword{datasets}
