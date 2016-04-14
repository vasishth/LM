## ----include=FALSE-------------------------------------------------------
library(knitr)
# set global chunk options, put figures into folder
options(replace.assign=TRUE,show.signif.stars=FALSE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')
options(show.signif.stars=FALSE)
library(lme4)

## ----cdfbinomial---------------------------------------------------------
## sample size
n<-10
## prob of success
p<-0.5
probs<-rep(NA,11)
for(x in 0:10){
  ## Cumulative Distribution Function:
probs[x+1]<-round(pbinom(x,size=n,prob=p),digits=2)
}

## ----echo=TRUE-----------------------------------------------------------
## Plot the CDF:
plot(1:11,probs,xaxt="n",
     xlab="Prob(X<=x)",
     main="CDF")
axis(1,at=1:11,labels=0:10)

## ------------------------------------------------------------------------
pbinom(1,size=10,prob=0.5)-pbinom(0,size=10,prob=0.5)
choose(10,1) * 0.5 * (1-0.5)^9

## ----pdfbinomial---------------------------------------------------------
## P(X=0)
dbinom(0,size=10,prob=0.5)

## ------------------------------------------------------------------------
## Plot the pdf:
plot(1:11,
     dbinom(0:10,size=10,prob=0.5),
     main="PDF",
     xaxt="n")
axis(1,at=1:11,labels=0:10)

## ----normaldistr,echo=FALSE,fig.width=6----------------------------------
plot(function(x) dnorm(x), -3, 3,
      main = "Normal density",ylim=c(0,.4),
              ylab="density",xlab="X")

## ------------------------------------------------------------------------
pnorm(Inf)-pnorm(-Inf)
pnorm(2)-pnorm(-2)
pnorm(1)-pnorm(-1)

## ------------------------------------------------------------------------
pnorm(2)

## ------------------------------------------------------------------------
x<-0:10
## expectation in our binomial example:
sum(x*dbinom(x,size=10,prob=0.5))

## ----gamma,echo=FALSE,fig.width=6----------------------------------------
## fn refers to the fact that it 
## is a function in R, it does not mean that 
## this is the gamma function:
gamma.fn<-function(x){
  lambda<-1
	alpha<-1
	(lambda * exp(1)^(-lambda*x) * 
	(lambda*x)^(alpha-1))/gamma(alpha)
}

x<-seq(0,4,by=.01)

plot(x,gamma.fn(x),type="l")

## ----chisq,echo=FALSE,fig.width=6----------------------------------------
gamma.fn<-function(x){
  lambda<-1/2
	alpha<-8/2 ## n=4
	(lambda * (exp(1)^(-lambda*x)) * 
	(lambda*x)^(alpha-1))/gamma(alpha)
}

x<-seq(0,100,by=.01)

plot(x,gamma.fn(x),type="l")

## ------------------------------------------------------------------------
(x<-rbinom(3,size=10,prob=0.5))

## ----likfun0,echo=TRUE,fig.width=6---------------------------------------
## probability parameter fixed at 0.5
theta<-0.5
prod(dbinom(x,size=10,prob=theta))
## probability parameter fixed at 0.1
theta<-0.1
prod(dbinom(x,size=10,prob=theta))
## probability parameter fixed at 0.9
theta<-0.9
prod(dbinom(x,size=10,prob=theta))

## let's compute the product for 
## a range of probabilities:
theta<-seq(0,1,by=0.01)
store<-rep(NA,length(theta))
for(i in 1:length(theta)){
store[i]<-prod(dbinom(x,size=10,prob=theta[i]))
}

plot(1:length(store),store,xaxt="n",xlab="theta",
     ylab="f(x1,...,xn|theta")
axis(1,at=1:length(theta),labels=theta)

## ------------------------------------------------------------------------
(x<-rbinom(3,size=10,prob=0.1))

## ----likfun,echo=TRUE,fig.width=6----------------------------------------
theta<-seq(0,1,by=0.01)
store<-rep(NA,length(theta))
for(i in 1:length(theta)){
store[i]<-prod(dbinom(x,size=10,prob=theta[i]))
}

plot(1:length(store),store,xlab="theta",
     ylab="f(x1,...,xn|theta",xaxt="n")
axis(1,at=1:length(theta),labels=theta)

## ----echo=FALSE,include=FALSE--------------------------------------------
#hindi10<-read.table("datacode/hindi10.txt",header=T)
#hindi10a<-hindi10[,c(1,3,13,22,24,25,26,27,28,29,32,33)]
#write.table(hindi10a,file="datacode/hindi10a.txt")

## ----echo=FALSE----------------------------------------------------------
hindi10<-read.table("datacode/hindi10a.txt",header=T)

colnames(hindi10)
summary(hindi10$TFT)
hindi10<-subset(hindi10,TFT>0)
summary(hindi10$TFT)

## ------------------------------------------------------------------------
hist(log(hindi10$TFT),freq=FALSE)

## ------------------------------------------------------------------------
(xbar<-mean(log(hindi10$TFT)))
(xvar<-var(log(hindi10$TFT)))

## ------------------------------------------------------------------------
xvals<-seq(0,12,by=0.01)
plot(xvals,dnorm(xvals,
                  mean=xbar,
                 sd=sqrt(xvar)),
     type="l",ylab="density",xlab="x")

## ----empdist,echo=TRUE,fig.width=6---------------------------------------
## The empirical distribution and 
## our theoretical distribution:
hist(log(hindi10$TFT),freq=FALSE)
xvals<-seq(0,4000,by=0.01)
lines(xvals,dnorm(xvals,
                 mean=xbar,sd=sqrt(xvar)))

## ----solutionex1,echo=FALSE,include=FALSE--------------------------------
xbar2<-mean(hindi10$TFT)
xvar2<-var(hindi10$TFT)
hist(hindi10$TFT,freq=FALSE)
lines(xvals,dnorm(xvals,
                 mean=xbar2,sd=sqrt(xvar2)))
## Sample distrn is truncated at 0.

## ------------------------------------------------------------------------
## define negative log lik:
nllh.normal<-function(theta,data){ 
  ## mean and sd
  m<-theta[1] 
  s<-theta[2] 
  x <- data
  n<-length(x) 
  logl<- sum(dnorm(x,mean=m,sd=s,log=TRUE))
  ## return negative log likelihood:
  -logl
  }

## example output:
nllh.normal(theta=c(40,4),log(hindi10$TFT))

## find the MLEs using optim:
## need to specify some starting values:
opt.vals.default<-optim(theta<-c(500,50),
                        nllh.normal,
      data=log(hindi10$TFT),
      hessian=TRUE)

## result of optimization:
(estimates.default<-opt.vals.default$par)

## compare with MLE:
xbar
## bias corrected sd:
sqrt(xvar)

## ----sampleexp,fig.width=6-----------------------------------------------
n_rep<-1000
samp_distrn_mean<-rep(NA,n_rep)
for(i in 1:n_rep){
x<-rexp(1000)
samp_distrn_mean[i]<-mean(x)
}

op<-par(mfrow=c(1,2),pty="s")
hist(x,xlab="x",ylab="density",freq=FALSE,main="Exponentially distributed data")
hist(samp_distrn_mean,xlab="x",ylab="density",freq=FALSE,
     main="Sampling distribution of mean")

## ----sampunif,fig.width=6------------------------------------------------
n_rep<-1000
samp_distrn_mean<-rep(NA,n_rep)
for(i in 1:n_rep){
x<-runif(1000)
samp_distrn_mean[i]<-mean(x)
}

op<-par(mfrow=c(1,2),pty="s")
hist(x,xlab="x",ylab="density",freq=FALSE,main ="Sampling from uniform")
hist(samp_distrn_mean,xlab="x",ylab="density",freq=FALSE,
     main="Sampling from uniform")

## ----ratesofchange,echo=F,fig.width=6------------------------------------
op<-par(mfrow=c(1,2),pty="s")

plot(function(x) dnorm(x,log=F,sd=0.001), -3, 3,
      main = "Normal density",#ylim=c(0,.4),
              ylab="density",xlab="X")
plot(function(x) dnorm(x,log=F,sd=10), -3, 3,
      main = "Normal density",#ylim=c(0,.4),
              ylab="density",xlab="X")

## ----estimatedSE,fig.width=6---------------------------------------------
## analytic calculation of SE from a single expt:
## number of heads in 100 coin tosses:
n<-100
p<-0.5
(x<-rbinom(1,n=n,prob=p))
hat_p <- sum(x)/n
(SE_2<-(hat_p*(1-hat_p))/n)
(SE<-sqrt(SE_2))

## by repeated sampling:
samp_distrn_means<-rep(NA,1000)
for(i in 1:1000){
  x<-rbinom(1,n=n,prob=p)
  samp_distrn_means[i]<-sum(x)/n  
}
hist(samp_distrn_means,xlab="x",ylab="density",
     freq=F,main="The sampling distribution (binomial)")
## this is the SE of the SDSM:
sd(samp_distrn_means)

## ----samplingdistrnmeans_setup_variables,echo=FALSE----------------------
nsim<-1000
n<-100
mu<-500
sigma<-100

## ----samplingdistrnmeans_runloop-----------------------------------------
nsim<-1000
n<-100
mu<-500
sigma<-100

samp_distrn_means<-rep(NA,nsim)
samp_distrn_var<-rep(NA,nsim)
for(i in 1:nsim){
  x<-rnorm(n,mean=mu,sd=sigma)
  samp_distrn_means[i]<-mean(x)
  samp_distrn_var[i]<-var(x)
}

## ----samplingdistrnmeans_fig,fig.width=6,echo=FALSE----------------------
op<-par(mfrow=c(1,2),pty="s")
hist(samp_distrn_means,main="Samp. distrn. means",
     freq=F,xlab="x",ylab="density")
hist(samp_distrn_var,main="Samp. distrn. sd",
     freq=F,xlab="x",ylab="density")

## ------------------------------------------------------------------------
## estimate from simulation:
sd(samp_distrn_means)
## estimate from a single sample of size n:
sigma/sqrt(n)

## ----variancesdsm--------------------------------------------------------
## estimate from simulation:
sd(samp_distrn_var)
## theoretical value:
(sqrt(2)*sigma^2)/sqrt(n)

## ----confint1------------------------------------------------------------
## lower bound:
mu-(2*sigma/sqrt(n))
## upper bound:
mu+(2*sigma/sqrt(n))

## ----confint2,fig.width=6------------------------------------------------
lower<-rep(NA,nsim)
upper<-rep(NA,nsim)
for(i in 1:nsim){
  x<-rnorm(n,mean=mu,sd=sigma)
  lower[i]<-mean(x) - 2 * sd(x)/sqrt(n)
  upper[i]<-mean(x) + 2 * sd(x)/sqrt(n)
}
## check how many CIs contain mu:
CIs<-ifelse(lower<mu & upper>mu,1,0)
table(CIs)
## 95% CIs contain true mean:
table(CIs)[2]/sum(table(CIs))

## ------------------------------------------------------------------------
(X<-matrix(c(rep(1,8),rep(c(-1,1),each=4),
            rep(c(-1,1),each=2,2)),ncol=3))
library(Matrix)
## full rank:
rankMatrix(X)
## det non-zero:
det(t(X)%*%X)

## ------------------------------------------------------------------------
y<-as.matrix(hindi10$TFT)
x<-log(hindi10$word_len)
m0<-lm(y~x)

## design matrix:
X<-model.matrix(m0)
head(X,n=4)
## (X^TX)^{-1}
invXTX<-solve(t(X)%*%X)
## estimated beta:
(beta<-invXTX%*%t(X)%*%y)

## estimated variance of beta:
(hat_sigma<-summary(m0)$sigma)
(hat_var<-hat_sigma^2*invXTX)

## ------------------------------------------------------------------------
## hat rho:
-21.61/(sqrt(31.36)*sqrt(16.88))

## ------------------------------------------------------------------------
round(summary(m0)$coefficients[,1:3],
             digits=3)

## ----tvsnormal,fig.width=6-----------------------------------------------
range <- seq(-4,4,.01)  
 
op<-par(mfrow=c(2,2),pty="s")

 for(i in c(2,5,15,20)){
   plot(range,dnorm(range),type="l",lty=1,
        xlab="",ylab="",
        cex.axis=1)
   lines(range,dt(range,df=i),lty=2,lwd=1)
   mtext(paste("df=",i),cex=1.2)
 }

## ------------------------------------------------------------------------
summary(m0)$coef

## ------------------------------------------------------------------------
2*pnorm(210.78,mean=0,sd=sqrt(31.36),
        lower.tail=FALSE)

2*pt(210.78/sqrt(31.36),df=length(y)-1,
   lower.tail=FALSE)

## ----typesandm,cache=TRUE,echo=TRUE--------------------------------------
## probable effect size derived from past studies:
D<-15
## SE from the study of interest:
se<-46
stddev<-se*sqrt(37)
nsim<-10000
drep<-rep(NA,nsim)
for(i in 1:nsim){
drep[i]<-mean(rnorm(37,mean=D,sd=stddev))
}

##power: a depressingly low 0.056
pow<-mean(ifelse(abs(drep/se)>2,1,0))

## which cells in drep are significant at alpha=0.05?
signif<-which(abs(drep/se)>2)

## Type S error rate | signif: 19%
types_sig<-mean(drep[signif]<0)
## Type S error rate | non-signif: 37%
types_nonsig<-mean(drep[-signif]<0)

## Type M error rate | signif: 7
typem_sig<-mean(abs(drep[signif])/D)
## Type M error rate | not-signif: 2.3 
typem_nonsig<-mean(abs(drep[-signif])/D)

## ------------------------------------------------------------------------
x<-1:10
y<- 10 + 2*x+rnorm(10,sd=10)

## ----simulatelm,fig.width=6----------------------------------------------
plot(x,y)

## ------------------------------------------------------------------------
## null hypothesis model:
m0<-lm(y~1)
## alternative hypothesis model:
m1<-lm(y~x)

## ------------------------------------------------------------------------
lambda<- -2*(logLik(m0)-logLik(m1))
## observed value:
lambda[1]
## critical value:
qchisq(0.95,df=1)
# p-value:
pchisq(lambda[1],df=1,lower.tail=FALSE)

## ------------------------------------------------------------------------
anova(m0,m1)

## ------------------------------------------------------------------------
sqrt(anova(m0,m1)$F[2])
summary(m1)$coefficients[2,3]

## ------------------------------------------------------------------------
X<-matrix(rep(1,10),ncol=1)
##
t(X)%*%X

## ------------------------------------------------------------------------
library(car)
vif(lm(TFT~syll_len+word_len,hindi10))

## ------------------------------------------------------------------------
m<-lm(TFT ~ word_complex + word_freq + type_freq+ 
         word_bifreq + type_freq+ 
         word_len + IC + SC,
       hindi10)
summary(m)
round(vif(m),digits=3)

## ----residualslm,fig.width=6---------------------------------------------
library(car)
qqPlot(residuals(m))

## ----normalityresiduals,fig.width=6--------------------------------------
op<-par(mfrow=c(1,2),pty="s")
x<-1:100
y1<- 10 + 2*x+rchisq(100,df=1)
qqPlot(residuals(lm(y1~x)))
y2<- 10 + 2*x+rnorm(100,sd=10)
qqPlot(residuals(lm(y2~x)))

## ------------------------------------------------------------------------
nsim<-1000
n<-100
x<-1:n
store_y1_results<-rep(NA,nsim)
store_y2_results<-rep(NA,nsim)
for(i in 1:nsim){
  e<-rchisq(n,df=1)
  e<-scale(e,scale=F)
  y1<- 10 + 0.01*x + e
  m1<-lm(y1~x)
  store_y1_results[i]<-summary(m1)$coefficients[2,4]
  y2<- 10 + 0.01*x + rnorm(n,sd=1.2)
  m2<-lm(y2~x)
  store_y2_results[i]<-summary(m2)$coefficients[2,4]
}

## power
y1_results<-table(store_y1_results<0.05)
y1_results[2]/sum(y1_results)

y2_results<-table(store_y2_results<0.05)
y2_results[2]/sum(y2_results)

## ----acftest,fig.width=6-------------------------------------------------
acf(residuals(m))

## ----lmdiagnostics,fig.width=6-------------------------------------------
op<-par(mfrow=c(2,2),pty="s")
plot(m)

## ----boxcox1,fig.width=6-------------------------------------------------
## generate some non-normally distributed data:
data<-rchisq(100,df=1)
m<-lm(data~1)  
qqPlot(residuals(m))

## ----boxcox2,fig.width=6-------------------------------------------------
library(MASS)
## suggests log:
boxcox(m)

m<-lm(log(data)~1)  

## ------------------------------------------------------------------------
(beetle<-read.table("datacode/beetle.txt",header=TRUE))

## ------------------------------------------------------------------------
(beetle$propn.dead<-beetle$killed/beetle$number)

## ------------------------------------------------------------------------
with(beetle,plot(dose,propn.dead))

## ------------------------------------------------------------------------
fm<-lm(propn.dead~scale(dose,scale=FALSE),beetle)
summary(fm)

## ------------------------------------------------------------------------
with(beetle,plot(scale(dose,scale=FALSE),
                 propn.dead))
abline(coef(fm))

## ------------------------------------------------------------------------
fm1<-glm(propn.dead~dose,
         binomial(logit),
         weights=number,
         data=beetle)
summary(fm1)

## ----propndeadplot,fig.width=6-------------------------------------------
plot(propn.dead~dose,beetle)
points(fm1$fitted~dose,beetle,pch=4)

## ------------------------------------------------------------------------
## compute log odds of death for 
## concentration 1.7552:
x<-as.matrix(c(1, 1.7552))
#log odds:
(log.odds<-t(x)%*%coef(fm1))

## ------------------------------------------------------------------------
### compute CI for log odds:
## Get vcov matrix:
(vcovmat<-vcov(fm1))
## x^T VCOV x for dose 1.7552:
(var.log.odds<-t(x)%*%vcovmat%*%x)

## ------------------------------------------------------------------------
##lower
log.odds-1.96*sqrt(var.log.odds)
##upper
log.odds+1.96*sqrt(var.log.odds)

## ------------------------------------------------------------------------
## eta=xbeta:
eta.i<- -60+35*beetle$dose

## ------------------------------------------------------------------------
n.i <- beetle$number
w.ii.fn<-function(n.i,eta.i){
  (n.i*exp(eta.i))/(1+exp(eta.i))^2
}
w.iis<-w.ii.fn(n.i,eta.i)
##weights matrix:
W<-diag(as.vector(w.iis))

## ------------------------------------------------------------------------
mu.i<-exp(eta.i)/(1+exp(eta.i))
z.i<-eta.i + ((beetle$propn.dead-mu.i))/
              (mu.i*(1-mu.i))

## ------------------------------------------------------------------------
##The design matrix:
col1<-c(rep(1,8))
X<-as.matrix(cbind(col1,beetle$dose))
## update coefs:
eta.i<-solve(t(X)%*%W%*%X)%*%
               t(X)%*%W%*%z.i

## ------------------------------------------------------------------------
glm1<-glm(propn.dead~dose,binomial(logit),
          weights=number,data=beetle)

## ------------------------------------------------------------------------
summary(glm1)

## ----propndead2,fig.width=6----------------------------------------------
# beta.hat is (-60.71745 ,   34.27033)
(eta.hat<-  -60.71745 +   34.27033*beetle$dose)
(mu.hat<-exp(eta.hat)/(1+exp(eta.hat)))

# compare mu.hat with observed proportions
plot(mu.hat,beetle$propn.dead)
abline(0,1)

## ----propndead3,fig.width=6----------------------------------------------
null.glm<-glm(propn.dead~1,binomial(logit),
          weights=number,data=beetle)
summary(null.glm)

plot(beetle$dose,beetle$propn.dead,xlab="log concentration",
    ylab="proportion dead",main="minimal model")
points(beetle$dose,null.glm$fitted,pch=4)

## ----propndead4,fig.width=6----------------------------------------------
dose.glm<-glm(propn.dead~dose,binomial(logit),
          weights=number,data=beetle)
summary(dose.glm)
plot(beetle$dose,beetle$propn.dead,xlab="log concentration",
    ylab="proportion dead",main="dose model")
points(beetle$dose,dose.glm$fitted,pch=4)

## ------------------------------------------------------------------------
anova(null.glm,dose.glm)

## ------------------------------------------------------------------------
anova(dose.glm)

## ------------------------------------------------------------------------
deviance(null.glm)
## critical value:
qchisq(0.95,df=7)

## ------------------------------------------------------------------------
deviance(dose.glm)
qchisq(0.95,df=6)

## ----residualsglm,fig.width=6--------------------------------------------
op<-par(mfrow=c(2,2),pty="s")
plot(dose.glm)

## ----qqnormglm,fig.width=6-----------------------------------------------
op<- par(mfrow=c(2,2),pty="s")
plot(dose.glm$resid,
     xlab="index",ylab="residuals",main="Index plot")
qqnorm(dose.glm$resid,main="QQ-plot")
hist(dose.glm$resid,xlab="Residuals",main="Histogram")
plot(dose.glm$fit,dose.glm$resid,xlab="Fitted values",
     ylab="Residuals",
     main="Residuals versus fitted values")

## ----loadnoisedeg--------------------------------------------------------
noisedeg<-read.table("datacode/noisedeg.txt")

## ------------------------------------------------------------------------
## returning to our noise data (noisedeg):
## here's an important fact about our data:
# different subjects have different means for no.noise and noise
# and different means for the three levels of deg

t(means.noise<-with(noisedeg,tapply(rt,list(subj,noise),mean)))

t(means.deg<-with(noisedeg,tapply(rt,list(subj,deg),mean)))

## ----xyplotnoisedeg,fig.width=6------------------------------------------
## We can visualize these differences graphically:

library(lattice)

## noise by subject (data points):
print(xyplot(rt~noise|subj,
        panel=function(x,y,...){panel.xyplot(x,y,type="r")},noisedeg))

## ----xyplotnoisedeg2,fig.width=6-----------------------------------------
## same as above, but for deg:
print(xyplot(rt~deg|subj,
        panel=function(x,y,...){panel.xyplot(x,y,type="r")},noisedeg))

## ------------------------------------------------------------------------
## fit a separate linear model for subject s1:
s1data<-subset(noisedeg,subj=="s1")
lm(rt~noise,s1data)

## ------------------------------------------------------------------------
## do the same for each subject using a for-loop
subjects<-paste("s",rep(1:10),sep="")
for(i in subjects){
  sdata<-subset(noisedeg,subj==i)
        lm(rt~noise,sdata)
}

## ------------------------------------------------------------------------
library(lme4)
lmlist.fm1<-lmList(rt~noise|subj,noisedeg)
print(lmlist.fm1$s1)

## ----noisedegplot,fig.width=6--------------------------------------------
plot(as.numeric(noisedeg$noise)-1,
     noisedeg$rt,axes=F,
     xlab="noise",ylab="rt")
axis(1,at=c(0,1),
     labels=c("no.noise","noise"))
axis(2)

subjects<-paste("s",1:10,sep="")

for(i in subjects){
abline(lmlist.fm1[[i]])
}

abline(lm(rt~noise,noisedeg),lwd=3,col="red")

## ------------------------------------------------------------------------
t.test(coef(lmlist.fm1)[2])

## ------------------------------------------------------------------------
## the following command fits a linear model, 
## but in addition estimates between-subject variance:
summary(m0.lmer<-lmer(rt~noise+(1|subj),noisedeg))

## ------------------------------------------------------------------------
ranef(m0.lmer)

## ----ranefsplot,fig.width=6----------------------------------------------
print(dotplot(ranef(m0.lmer,condVar=TRUE)))

## ----ranefsnoisedeg,fig.width=6------------------------------------------
a<-fixef(m0.lmer)[1]
newa<-a+ranef(m0.lmer)$subj

ab<-data.frame(newa=newa,b=fixef(m0.lmer)[2])

plot(as.numeric(noisedeg$noise)-1,noisedeg$rt,xlab="noise",ylab="rt",axes=F)
axis(1,at=c(0,1),labels=c("no.noise","noise"))
axis(2)
for(i in 1:10){
abline(a=ab[i,1],b=ab[i,2])
}
abline(lm(rt~noise,noisedeg),lwd=3,col="red")

## ------------------------------------------------------------------------
summary(m1.lmer<-lmer(rt~noise+(1+noise|subj),noisedeg))

## ----ranefsnoisedeg2,fig.width=6-----------------------------------------
(a<-fixef(m1.lmer)[1])
(b<-fixef(m1.lmer)[2])

newa<-a+ranef(m1.lmer)$subj[1]
newb<-b+ranef(m1.lmer)$subj[2]
## make this into a data frame:
ab<-data.frame(newa=newa,b=newb)

plot(as.numeric(noisedeg$noise)-1,noisedeg$rt,xlab="noise",ylab="rt",axes=F,
main="varying intercepts and slopes for each subject")
axis(1,at=c(0,1),labels=c("no.noise","noise"))
axis(2)

for(i in 1:10){
abline(a=ab[i,1],b=ab[i,2])
}

abline(lm(rt~noise,noisedeg),lwd=3,col="red")

## ----echo=FALSE,fig.width=6----------------------------------------------
op<- par(mfrow=c(1,2),pty="s")

plot(as.numeric(noisedeg$noise)-1,noisedeg$rt,axes=F,xlab="noise",ylab="rt",main="ordinary linear model")
axis(1,at=c(0,1),labels=c("no.noise","noise"))
axis(2)

subjects<-paste("s",1:10,sep="")

for(i in subjects){
abline(lmlist.fm1[[i]])
}

abline(lm(rt~noise,noisedeg),lwd=3,col="red")

a<-fixef(m1.lmer)[1]
b<-fixef(m1.lmer)[2]

newa<-a+ranef(m1.lmer)$subj[1]
newb<-b+ranef(m1.lmer)$subj[2]

ab<-data.frame(newa=newa,b=newb)

plot(as.numeric(noisedeg$noise)-1,noisedeg$rt,axes=F,
main="varying intercepts and slopes",xlab="noise",ylab="rt")
axis(1,at=c(0,1),labels=c("no.noise","noise"))
axis(2)

for(i in 1:10){
abline(a=ab[i,1],b=ab[i,2])
}

abline(lm(rt~noise,noisedeg),lwd=3,col="red")

## ------------------------------------------------------------------------
m<-lmer(rt~noise + (1+noise|subj),noisedeg)
summary(m)

## ------------------------------------------------------------------------
contrasts(noisedeg$noise)
## set to sum contrasts:
contrasts(noisedeg$noise)<-contr.sum(2)
contrasts(noisedeg$noise)
m<-lmer(rt~noise + (1+noise|subj),noisedeg)
summary(m)

## ------------------------------------------------------------------------
c1<-ifelse(noisedeg$noise=="noise",-1,1)
m<-lmer(rt~c1 + (c1||subj),noisedeg)
summary(m)

## ------------------------------------------------------------------------
BHHshoes<-read.table("datacode/BHHshoes.txt")
lm.full<-lmer(wear~material-1+
                (1|Subject), 
              data = BHHshoes)

## ------------------------------------------------------------------------
b1.vals<-subset(BHHshoes,
                material=="A")$wear
b2.vals<-subset(BHHshoes,
                material=="B")$wear

vcovmatrix<-var(cbind(b1.vals,b2.vals))

## get covariance from off-diagonal:
covar<-vcovmatrix[1,2]
sds<-sqrt(diag(vcovmatrix))
## correlation of fixed effects:
covar/(sds[1]*sds[2])

#cf:
covar/((0.786*sqrt(10))^2)  

## ------------------------------------------------------------------------
dbinom(46, 100, 0.5)

## ----betaeg,echo=F,fig.width=6-------------------------------------------
plot(function(x) 
  dbeta(x,shape1=2,shape2=2), 0,1,
      main = "Beta density",
              ylab="density",xlab="X",ylim=c(0,3))

text(.5,1.1,"a=2,b=2")

plot(function(x) 
  dbeta(x,shape1=3,shape2=3),0,1,add=T)
text(.5,1.6,"a=3,b=3")

plot(function(x) 
  dbeta(x,shape1=6,shape2=6),0,1,add=T)
text(.5,2.6,"a=6,b=6")

## ----binomplot,echo=F,fig.width=6----------------------------------------
theta=seq(0,1,by=0.01)

plot(theta,dbinom(x=46,size=100,prob=theta),
     type="l",main="Likelihood")

## ----betaforbinom,echo=F,fig.width=6-------------------------------------
plot(function(x) 
  dbeta(x,shape1=46,shape2=54),0,1,
              ylab="",xlab="X")

## ----binomexample1,echo=F,fig.width=6------------------------------------
##lik:
plot(function(x) 
  dbeta(x,shape1=46,shape2=54),0,1,
              ylab="",xlab="X",col="red")

## prior:
plot(function(x) 
  dbeta(x,shape1=2,shape2=2), 0,1,
      main = "Prior",
              ylab="density",xlab="X",add=T,lty=2)

## posterior
plot(function(x) 
  dbeta(x,shape1=48,shape2=56), 0,1,
      main = "Posterior",
              ylab="density",xlab="X",add=T)

legend(0.1,6,legend=c("post","lik","prior"),
       lty=c(1,1,2),col=c("black","red","black"))

## ------------------------------------------------------------------------
y<-1
n<-1

thetas<-seq(0.2,0.8,by=0.2)

likelihoods<-rep(NA,4)
for(i in 1:length(thetas)){
  likelihoods[i]<-dbinom(y,n,thetas[i])  
}

## ------------------------------------------------------------------------
sum(likelihoods)

## ------------------------------------------------------------------------
(priors<-rep(0.25,4))

## ------------------------------------------------------------------------
liks.times.priors<-likelihoods * priors

## normalizing constant:
sum.lik.priors<-sum(liks.times.priors)

posteriors<- liks.times.priors/sum.lik.priors

## ------------------------------------------------------------------------
n<-20
y<-15

priors<-rep(0.25,4)

likelihoods<-rep(NA,4)
for(i in 1:length(thetas)){
  likelihoods[i]<-dbinom(y,n,thetas[i])  
}

liks.priors<-likelihoods * priors

sum.lik.priors<-sum(liks.priors)

(posteriors<- liks.priors/sum.lik.priors)

## ------------------------------------------------------------------------
posteriors

## ------------------------------------------------------------------------
thetas<-seq(0,1,by=0.2)
priors<-rep(1/6,6)

y<-15
n<-20

likelihoods<-rep(NA,6)
for(i in 1:length(thetas)){
  likelihoods[i]<-dbinom(y,n,thetas[i])  
}

liks.priors<-likelihoods * priors

sum.lik.priors<-sum(liks.priors)

(posteriors<- liks.priors/sum.lik.priors)

## ------------------------------------------------------------------------
thetas<-seq(0,1,by=0.2)
priors<-rep(1/6,6)

y<-1
n<-1

j<-6 ## no. of thetas
likelihoods<-rep(NA,6)
for(i in 1:length(thetas)){
  likelihoods[i]<-dbinom(y,n,thetas[i])  
}

liks.priors<-likelihoods * priors

sum.lik.priors<-sum(liks.priors)

posteriors<- liks.priors/sum.lik.priors

## ----echo=F--------------------------------------------------------------
x<-seq(0,1,length=100)
plot(x,dbeta(x,shape1=9.2,shape2=13.8),type="l")

## ----echo=F--------------------------------------------------------------
thetas<-seq(0,1,length=100)
probs<-rep(NA,100) 

for(i in 1:100){
probs[i]<-dbinom(15,20,thetas[i])
}

plot(thetas,probs,main="Likelihood of y|theta_j",type="l")

## ----likbetaexample2,echo=F,fig.width=6----------------------------------
x<-seq(0,1,length=100)
plot(x,dbeta(x,shape1=15,shape2=5),type="l")

## ----fig.keep='none',echo=F----------------------------------------------
thetas<-seq(0,1,length=100)
a.star<-9.2+15
b.star<-13.8+5
plot(thetas,dbeta(thetas,
                  shape1=a.star,
                  shape2=b.star),
     type="l")

## ----fig.keep='none',echo=F----------------------------------------------
par(mfrow=c(3,1))

## prior
plot(thetas,dbeta(x,shape1=9.2,shape2=13.8),
     type="l",
     main="Prior")

## lik
probs<-rep(NA,100) 

for(i in 1:100){
probs[i]<-dbinom(15,20,thetas[i])
}

plot(thetas,probs,main="Likelihood of y|theta_j",type="l")

## post
x<-seq(0,1,length=100)

a.star<-9.2+15
b.star<-13.8+5

plot(x,dbeta(x,shape1=a.star,shape2=b.star),type="l",
     main="Posterior")

## ----echo=F,include=F----------------------------------------------------
plot.it<-function(m=0.4,s=0.1,k=15,n=20){
  ## compute a,b
  a.plus.b<-((m*(1-m))/s^2)-1
  a<-a.plus.b*m
  b<-a.plus.b-a
  
  ##prior
thetas<-seq(0,1,length=100)
plot(thetas,dbeta(thetas,shape1=a,shape2=b),type="l",main="",ylab="")

probs<-dbinom(k,n,thetas)  
lines(thetas,probs,type="l",lty=2)
  
## post
a.star<-a+k
b.star<-b+(n-k)

lines(thetas,dbeta(thetas,shape1=a.star,shape2=b.star),lty=3,lwd=3,type="l")
}

plot.it()

plot.it(m=0.5,s=0.4,k=15,n=20)

## ----fig1,echo=F,fig.width=6---------------------------------------------
x<-0:200
plot(x,dgamma(x,10000/225,100/225),type="l",lty=1,main="Gamma prior",ylab="density",cex.lab=2,cex.main=2,cex.axis=2)

## ------------------------------------------------------------------------
## load data:
data<-c(115,97,79,131)

a.star<-function(a,data){
  return(a+sum(data))
}

b.star<-function(b,n){
  return(b+n)
}

new.a<-a.star(10000/225,data)
new.b<-b.star(100/225,length(data))

## post. mean
post.mean<-new.a/new.b 
## post. var:
post.var<-new.a/(new.b^2) 

new.data<-c(200)

new.a.2<-a.star(new.a,new.data)
new.b.2<-b.star(new.b,length(new.data))

## new mean
new.post.mean<-new.a.2/new.b.2
## new var:
new.post.var<-new.a.2/(new.b.2^2)

## ----echo=T--------------------------------------------------------------
## specify data:
dat<-list(y=c(115,97,79,131))

## model specification:
cat("
model
   {
for(i in 1:4){
  y[i] ~ dpois(theta)
}
  ##prior
  ## gamma params derived from given info:  
  theta ~ dgamma(10000/225,100/225)
}",
    file="datacode/poissonexample.jag" )

## specify variables to track
## the posterior distribution of:
track.variables<-c("theta")

## load rjags library:
library(rjags,quietly=T)

## define model:
pois.mod <- jags.model( 
    data = dat,
    file = "datacode/poissonexample.jag",
    n.chains = 4,
    n.adapt =2000 ,quiet=T)

## run model:
pois.res <- coda.samples( pois.mod,
               var = track.variables,
               n.iter = 50000,
               thin = 50 ) 

## ------------------------------------------------------------------------
## summarize and plot:
plot(pois.res)

## ----echo=TRUE-----------------------------------------------------------
print(summary(pois.res))

## ----fig3,echo=F,fig.width=6---------------------------------------------
## lik: 
x<-0:200
plot(x,dpois(x,lambda=mean(dat$y)),type="l",ylim=c(0,.1),ylab="")

## normal approximation:
#lines(x,dnorm(x,mean=mean(dat$y),sd=sqrt(mean(dat$y))),lty=2,col="red",lwd=3)

## gamma for the likelihood:
#a/b=105.5, a/b^2=105.5
## a = 105.5*b and a=105.5*b^2
## 105.5*b = 105.5*b^2
## 105.5=105.5 * b -> b=1
## a=105.5, b=1
#lines(x,dgamma(x,shape=105.5,rate=1),
#      lty=1,col="red",lwd=3)

## prior: gamma(10000/225,100/225)
lines(0:200,dgamma(0:200,shape=10000/225, rate = 100/225),
      lty=2)

#posterior from JAGS:
lines(0:200,dgamma(0:200,shape=466.44, rate = 4.44),col="red",lwd=3)

legend(x=150,y=0.08,legend=c("lik","prior","post"),
       lty=c(1,2,1),col=c("black","red","red"))

## ------------------------------------------------------------------------
dat2<-list(y=c(115,97,79,131,200))

## model specification:
cat("
model
   {
for(i in 1:4){
  y[i] ~ dpois(theta)
}
  y[5] ~ dpois(2*theta)
    
  ##prior
  ## gamma params derived from given info:  
  theta ~ dgamma(10000/225,100/225)
}",
    file="datacode/poisexample2.jag" )

## specify variables to track
## the posterior distribution of:
track.variables<-c("theta")

## define model:
poisex2.mod <- jags.model( 
    data = dat2,
    file = "datacode/poisexample2.jag",
    n.chains = 4,
    n.adapt =2000 ,quiet=T)

## run model:
poisex2.res <- coda.samples( poisex2.mod,
               var = track.variables,
               n.iter = 100000,
               thin = 50 ) 

## ------------------------------------------------------------------------
print(summary(poisex2.res))

