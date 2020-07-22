library(lme4)
## load data
hindi10<-read.table("datacode/hindi10a.txt",header=TRUE)

## skipping: 1 if word is skipped, 0 otherwise
skip<-ifelse(hindi10$TFT==0,1,0)
hindi10$skip<-skip

summary(hindi10$word_complex)
## make a sum contrast for illustration
hindi10$complex<-ifelse(hindi10$word_complex<1,-1,1)

## fit linear mixed model on 0,1 responses
fm_skip<-glmer(skip ~ complex+(1|subj)+(1|item),
               family=binomial(),hindi10)

summary(fm_skip)

## generate fake data:
N<-dim(hindi10)[1]

dat<-hindi10[,c(1,2,5,13,14)]
head(dat)

## extract estimates:
b0<-summary(fm_skip)$coefficients[1,1]
b1<-summary(fm_skip)$coefficients[2,1]

## assemble variance covariance matrix for subjects:
subj_ranefsd<-attr(VarCorr(fm_skip)$subj,"stddev")
## only if there is a varying slope and a correlation is defined
#subj_ranefcorr<-round(attr(VarCorr(m)$subj,"corr"),1)
## choose some intermediate values for correlations:
#corr_matrix<-(diag(4) + matrix(rep(1,16),ncol=4))/2
#Sigma_u<-SIN::sdcor2cov(stddev=subj_ranefsd,corr=corr_matrix)

## assemble variance covariance matrix for items:
item_ranefsd<-attr(VarCorr(fm_skip)$item,"stddev")
#Sigma_w<-SIN::sdcor2cov(stddev=item_ranefsd,corr=corr_matrix)

skip_fake <- rep(NA,N)

nsubj<-length(unique(hindi10$subj))
nitem<-length(unique(hindi10$item))

nsim<-100
zval<-rep(NA,nsim)
## generate data many times:
for(j in 1:nsim){
  ## produce fake data:
for(i in 1:N){
  ## generate varying intercepts
  u0<-rnorm(nsubj,mean=0,sd=subj_ranefsd)
  w0<-rnorm(nitem,mean=0,sd=item_ranefsd)
  ## compute prediction on log odds scale
  mu_hat<-b0 + u0[dat[i,]$subj] + u0[dat[i,]$item] +  b1*dat[i,]$complex
  ## convert back to probability scale
  p_temp<-exp(mu_hat)/(1+exp(mu_hat))
  ## generate one sample from binomial: 
  skip_fake[i]<-rbinom(1,n=1,prob=p_temp)
}
  ## fit glmer model:
dat$skip_fake<-skip_fake
fm_skip_fake<-glmer(skip_fake ~ complex + 
                      (1|subj) + (1|item),family=binomial(),dat)
## save z-score:
zval[j]<-summary(fm_skip_fake)$coefficients[2,3]
}
## ignore singularities for now

## power:
mean(abs(zval)>2)
