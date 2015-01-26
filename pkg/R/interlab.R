interlabstats <- function(object, ...)
  UseMethod("interlabstats")

interlabstats.default <-
  function(object, ...)
  {
    if (!class(object)%in%c("lmerMod","rlmerMod"))
      stop("inadequate argument 'object'")
    
    lsum <- summary(object)
    ldata <- try(get(as.character(lsum$call$data)))
    if (class(ldata)=="try.error") {
      warning("I do not find the data. -> not included in result")
      ldata <- NULL } else {
      ldata <- ldata[,all.vars(lsum$call$formula)]
      if (NCOL(ldata)!=2) {
        warning("I do not find the variables. -> data not included in result")
        ldata <- NULL
      }
    }
    lmn <- lsum$coef["(Intercept)",1]
    lvc <- lsum$varcor
    lsigma <- lsum$sigma
    lsiggroup <- lvc[[1]][1,1]
    lrpt <- 2*sqrt(2)*lsigma
    lrpr <- 2*sqrt(2*(lsiggroup^2+lsigma^2))
    structure(
      list(mean=lmn, sigma=lsigma, siggroup=lsiggroup,
           repeatability=lrpt, reproducibility=lrpr,
           ## groups=lgroups,
           data=ldata, call=lsum$call, method=class(object)),
      class="interlab" )
  }

interlabstats.formula <- function(formula, data, subset)
{
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 19 Jan 2015, 12:38
  lcall <- match.call()
  mf <- match.call(expand.dots = FALSE)
  lj <- match(c("formula", "data", "subset"), # , "weights", "offset"
             names(mf), 0L)
  mf <- mf[c(1L, lj)]
  mf$formula <- as.formula(formula)
  mf[[1L]] <- as.name("model.frame")
  ldata <- eval(mf, parent.frame())
  if (!is.factor(ldata[[2]])) 
    warning("variable", names(ldata)[2], "is not a factor. I try to covert it.")
  ldata[,2] <- factor(ldata[,2])
  ltb <- table(ldata[,2])
  lina <- is.na(ldata[,1])
  ldt <- ldata[!lina,]
  if (any(lina)) {
    ltb <- cbind(all=ltb,notNA=ltb)
    ltb2 <- table(ldt[,2])
    ltb[names(ltb2),2] <- ltb2
  }
  ldtg <- split(ldt[,1],ldt[,2])
  lnk <- sapply(ldtg,length)
  lmk <- sapply(ldtg,mean)
  lvk <- sapply(ldtg,sd)
  lgroups <- cbind( n=lnk, mean=lmk, sd=sqrt(lvk) )
  lnn <- nrow(ldt)
  lng <- nrow(lgroups)
  lmn <- mean(ldt[,1])
  ldfrpr <- (lnn-sum(lnk^2)/lnn)/(lng-1)  # (11)
  lsigma2 <- sum((lnk-1)*lvk)/(lnn-lng)  # (6)
  lsiggroup2 <- max(0,
                    (sum(lnk*(lmk-lmn)^2)/(lng-1)-lsigma2) / ldfrpr ) # (10)
  lsigdiff2 <- lsigma2+lsiggroup2 # (12)
  lrpt <- 2*sqrt(2*lsigma2)
  lrpr <- 2*sqrt(2*lsigdiff2)
  structure(
    list(mean=lmn, sigma=sqrt(lsigma2), siggroup=sqrt(lsiggroup2),
         repeatability=lrpt, reproducibility=lrpr, groups=lgroups,
         data=ldata, call=lcall,
         method="classical"), class="interlab" )
}
## ===================================================================
plot.interlab <- function(x, data=NULL, sd=TRUE, difflimits=FALSE, ylim=NULL,
                          grouplabels=NULL, cex.text=1.1, col="blue", ...)
{
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 19 Jan 2015, 14:39
  if (is.null(data)) data <- x$data
  lfo <- as.formula(x$call$formula)
  lnames <- all.vars(lfo)
  lgr <- factor(data[,lnames[2]])
  llev <- levels(lgr)
  lng <- length(llev)
  if (difflimits) ylim <- range(data[,1],x$reproducibility)
  plot(data[,lnames[1]]~lgr, xlim=c(0.5,lng+0.4+1.5*(sd+difflimits)),
       xlab=lnames[2], ylab=lnames[1], ylim=ylim, axes=F, ...) 
  axis(2)
  llab <- if (is.null(grouplabels)) substring(llev,1,3) else
    grouplabels
  axis(1, at=1:lng, labels=llab)
  box()
  if ("groups"%in%names(x)) {
    lmns <- x$groups[,"mean"]
    segments((1:lng)-0.2,lmns,(1:lng)+0.2,lmns, lwd=3,col=col[1])
  }
  lmn <- x$mean
  abline(h=lmn,lty=3,col=col[1])
  ## --- 
  lxst <- lng
  ldelt <- 0.05*diff(par("usr")[3:4])
  if (sd) {
    lines(rep(lxst+1,2),lmn+c(-1,1)*x$sigma,lwd=3) # col=col[1],
    text(lxst+1,lmn+x$sigma+ldelt,"within lab",adj=0,srt=90,cex=cex.text)
    lines(rep(lxst+1.4,2),lmn+c(-1,1)*x$siggroup,col=col[1],lwd=3)
    text(lxst+1.4,lmn+x$siggroup+ldelt,"between labs",col=col[1],
         adj=0,srt=90,cex=cex.text)
    mtext(expression(symbol()%+-%hat(sigma)),
          1,1.6,at=6.2,adj=1)
    mtext(expression(symbol()%+-%hat(sigma)[A]),
          1,1.7,at=6.3, adj=0, col=col[1])
    lxst <- lxst+1.5
  }
  if (difflimits) { 
    lines(rep(lxst+1.4,2),lmn+c(-1,1)*x$repeatability,lwd=3)
    text(lxst+1.4, lmn+x$repeatability+ldelt, "repeatability",
         col=col[2],adj=0,srt=90,cex=cex.text)
    lines(rep(lxst+1.6,2),lmn+c(-1,1)*x$reproducibility,col=col[1],lwd=3)
    text(lxst+1.8, lmn+0.5*x$repeatability, "reproducibility",
         col=col[1],adj=0,srt=90,cex=cex.text)
#    mtext("2 st.dev.",1,1,at=6,col=2,cex=1.5)
  }
}

## ===================================================================
print.interlab <- function(x, groups=TRUE, ...) {
  cat(format(x$call)," \n   method = ",x$method,"\n")
  cat("   overall mean  = ",format(x$mean),
      " ; sigma = ",format(x$sigma),
      " ; sd.between = ",format(x$siggroup),"\n")
  cat("   repeatability = ",format(x$repeatability),
      " ; reproducibility = ",format(x$reproducibility),"\n")
  if (groups&&!is.null(x$groups)) {
    cat("\n groups:\n")
    print(x$groups)
  }
}
