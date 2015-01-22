\name{d.perm}
\alias{d.perm}
\docType{data}
\title{Air permeability of concrete. 
%%   ~~ data name/kind ... ~~
}
\description{Air permeability of 75 samples of concrete from two
  selected elements of a bridge, measured by 5 teams.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("d.perm")}
\format{
  A data frame with 150 observations on the following 5 variables.
  \describe{
    \item{\code{team}}{The team that performed the measurement. Levels \code{E} \code{H} \code{L} \code{S} \code{T}}
    \item{\code{section}}{Element of the bridge: section 1 or 2}
    \item{\code{rep}}{Repetition}
    \item{\code{permeability}}{Air permeability [$m^2$]}
    \item{\code{perm.log}}{\code{log10(permeability)}}
  }
}
%%\details{
%%}
\source{
  Jacobs, F., Leemann, A., Denari\'e, E. and Teruzzi, T. (2009).
  Recommendations for the quality control of concrete with air
  permeability measurements.
  Technical Report 641, Bundesamt f\"ur Strassen (ASTRA), Bern, Switzerland.
}
\references{
  Stahel, W.A., Moro F. and .
  ``Statistical Procedures for Performance-based Specification and
  Testing''.
  In: 
}
\examples{
data(d.perm)
showd(d.perm)
t.fac <- factor(paste(d.perm$team, d.perm$section, sep=":"))
plot(perm.log~t.fac, data=d.perm, notch=TRUE)

summary(aov(perm.log~team+section, data=d.perm))
}
\keyword{datasets}
