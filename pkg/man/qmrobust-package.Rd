\name{qmrobust-package}
\alias{qmrobust-package}
\alias{qmrobust}
\docType{package}
\title{
Quality management: some clssical and eventually robust methods and
datasets for quality management
}
\description{
The package currently only collects 3 datasets -- and a vignette
describing their analysis. It is a companion of the book chapter
given in the References Section.
We plan to include specific functions that are not contained in
other quality control packages, with an emphasis on robust methods.
}
\details{
\tabular{ll}{
Package: \tab qm.concrete\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-08\cr
License: \tab GPL\cr
}
The package collects 3 datasets.
}
\author{
  Werner A. Stahel, Seminar f\"ur Statistik, ETH Zurich,
  \email{stahel@stat.math.ethz.ch}
}
\references{
  Stahel, W.A., Moro F. and Luco, L.F. (2015).
  ``Statistical Procedures for Performance-based Specification and
  Testing''.
  In: 
}
\keyword{ package }
\seealso{
\code{\link[qcc:qcc-package]{qcc}},
\code{\link[qcr:qcr-package]{qcr}}
}
\examples{
data(d.perm)
showd(d.perm)
data(d.tunnel1)
showd(d.tunnel1)
data(d.tunnel2)
showd(d.tunnel2)
data(d.coverdepth)
showd(d.coverdepth)
}
