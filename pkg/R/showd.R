showd <-
function (data, first = 3, nrow. = 4, ncol. = NULL) 
{
 ##-    ldoc <- getUserOption("doc")
##-     if (length(ldoc) > 0 && ldoc && length(tit(data)) > 0) {
##-         cat("tit: ", tit(data), "\n")
##-     }
   if (length(dim(data)) > 0) {
        cat("dim: ", dim(data), "\n")
    }
    ldata <- cbind(data)
    l.nr <- nrow(ldata)
    l.nc <- ncol(ldata)
    l.ic <- if (length(ncol.) == 0) 
        1:l.nc
    else {
        if (length(ncol.) == 1) {
            if (l.nc > ncol.) 
                c(seq(1, by = l.nc%/%ncol., length = ncol. - 
                  1), l.nc)
            else 1:l.nc
        }
        else {
            lic <- ncol.[ncol. > 0 & ncol <= l.nc]
            if (length(lic) > 0) 
                lic
            else 1:l.nc
        }
    }
    if (l.nr <= nrow. + first) 
        l.dc <- format(ldata[, l.ic])
    else {
        l.ir <- c(1:first, round(seq(first, l.nr, length = nrow. + 
            1))[-1])
        l.ir <- unique(c(last(l.ir, -1), l.nr))
        l.dc <- data.frame(u.merge(format(ldata[l.ir, l.ic]), 
            "", after = first), stringsAsFactors = FALSE)
        names(l.dc) <- colnames(ldata)[l.ic]
        if (length(lrn <- row.names(ldata)) > 0) 
            row.names(l.dc) <- c(lrn[1:first], "...", lrn[l.ir[-(1:first)]])
    }
    if (l.nc == 1) {
        row.names(l.dc) <- format(rbind(row.names(l.dc), l.dc[, 
            1]), justify = "right")[1, ]
        l.dc <- t(l.dc)
    }
    print(l.dc, quote = FALSE)
##-     if (length(ldoc) && ldoc && length(doc(data))) 
##-         cat("\ndoc:  ", paste(doc(data), collapse = "\n  "), 
##-             "\n")
    invisible(l.dc)
}
## ============================================================
last <-
function(data,n = 1)
{
  ldt <- length(data)
  data[sign(n)*((ldt-abs(n)+1):ldt)]
}
## ============================================================

u.merge <- function(dd1, dd2 = NA, which=NULL, after=NULL,
                    length=NULL, names=NULL)
{
## Purpose:   merge two vectors or expand a vector by NA s
## -------------------------------------------------------------------------
## Arguments:
##   dd1      first vector or matrix or data.frame (?),
##   dd2      second vector, ...
##   which    is T for indices for which first vector is used
##   after    elements of  dd2  will be inserted after "after" in dd1
##   length   length of the result (will be expanded if necessary)
##   names    names of the result (if length is adequate)
## -------------------------------------------------------------------------
## Author: Werner Stahel, Date: 11 Mar 93, 13:50, and later
  llen <- length
  n1 <- length(dd1)
  nc1 <- ncol(dd1)
  nc2 <- ncol(dd2)
  if (length(nc1)>0) {
    n1 <- nrow(dd1)
    if (!( length(dd2)==1 || is.null(nc2) || nc2==nc1 ))
      stop("unsuitable second argument")
    }
## --- generate  which  vector for all cases
  if (length(which)==0) {
## - after  specified
      if (length(after)==0) stop("specify either  which  or  after")
      if (is.logical(after))  after <- which(after)
      wh <- rep(TRUE,n1+length(after))
      wh[after+1:length(after)] <- FALSE }
  else {
## - which  specified
    if(is.logical(which)) wh <- which
    else {
      if (length(llen)==0)  llen <- n1+length(which)
        wh <- rep(TRUE, llen)
        wh[which] <- FALSE }
  }
## --- merge
  nn <- length(wh)
  n2 <- nn-n1
  if (!(is.null(names)|length(names)==nn))
    warning("argument  names  not used (unsuitable length)")
  if (length(nc1)>0) {
    if (!(length(dd2)==1 || NROW(dd2)==n2))
      stop("unsuitable number of rows")
    rr <- matrix(NA,nn,nc1)
    rr[wh,] <- as.matrix(dd1)
    rr[!wh,] <- if (is.data.frame(dd2)) as.matrix(dd2) else dd2
##-     if (length(names)>0) row.names(rr) <- names else {
##-       if (length(lrn1 <- row.names(dd1))>0)
  }
  else {
    rr <- rep(NA,nn)
    rr[wh] <- dd1
    rr[!wh] <- dd2
    if (length(names)>0) names(rr) <- names
  }
  rr
}
## ============================================================
