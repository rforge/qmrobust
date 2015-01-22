require(regr0,lib="~/R/regdevelop/pkg/regr0.Rcheck")
## -----------------------------------------------------------------
load("../data/d.perm.rda")
##- d.perm$Wall <- paste("w",d.perm$Wall,sep=".")
t.fac <- factor(paste(d.perm$team, d.perm$section, sep=":"))
plot(perm.log~t.fac, data=d.perm, notch=T)
r.aov <- aov(perm.log~team+section, data=d.perm, na.action=na.exclude)
summary(r.aov)
summary(aov(sqrt(abs(residuals(r.aov)))~team, data=d.perm))

r.aov <- aov(perm.log~team*section+factor(rep):section, data=d.perm)
summary(r.aov)
with(d.perm[d.perm$section=="w.2",],
     interaction.plot(team, rep, perm.log)
     )
## save(d.perm, file="../data/d.perm.rda")
## -----------------------------------------------------------------
load("../data/d.tunnel1.rda")
showd(d.tunnel1)

load("../data/d.tunnel2.rda")
showd(d.tunnel2)

d.tunnel <- merge(d.tunnel1, d.tunnel2,
                  by=c("section","position","layer","n"),
                  all.x=TRUE, suffixes=c(".68",".50"))
d.tunnel <- d.tunnel[,-c(5,10)] # diameter
showd(d.tunnel)

plot(strength.50~strength.68,d.tunnel)
plot(density.50~density.68,d.tunnel)
r.cor <- cor(d.tunnel$density.50,d.tunnel$density.68, use="complete.obs")
text(2450,2250, paste("corr =", round(r.cor,2)))
r.lm <- regr(log10(pmax(1e-11,perm.O2))~log10(pores.H2O)+density.68+section,
           data=d.tunnel)
r.lm <- lm(log10(pmax(1e-11,perm.O2))~log10(pmax(cond.CL,0.5))+
             density.68+section,  data=d.tunnel)
summary(r.lm)
r.lmr <- regr(log10(pmax(1e-11,perm.O2))~log10(pmax(cond.CL,0.5))+
             density.68+section,  robust=T, data=d.tunnel)
plmatrix(log10(pmax(1e-11,perm.O2))~log10(pmax(cond.CL,0.5))+
         density.68+section, col=as.numeric(d.tunnel$section),
           data=d.tunnel) # 1+(r.lm$rweights<0.9),
plot(r.lmr, ask=T)
## save(d.tunnel1, file="../data/d.tunnel1.rda")
## save(d.tunnel2, file="../data/d.tunnel2.rda")
## -----------------------------------------------------------------
load("../data/d.coverdepth.rda")
showd(d.coverdepth)
dd <- d.coverdepth[d.coverdepth$section<=2,]
symbols(dd$column,dd$row, circles=dd$depth, inches=par("cin")[1],
        xlab="",ylab="")

## -----------------------------------------------------------------
require(outliers)

ff <- function(x) grubbs.test(x, type=20, two.sided=FALSE)$p.value

t.n <- 25
t.nrep <- 1000
ds <- matrix(rnorm(t.n*t.nrep),t.n)
t.r <- apply(ds, 2, ff)

hist(t.r)
sum(t.r<0.05)


require(lme4)
dd <- d.perm[d.perm$section=="w.1",]
r.lme <- lmer(perm.log~ (1|team), data=dd)
summary(r.lme)

