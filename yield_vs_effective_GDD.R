#remove all data/variables and plots
rm(list = ls())
graphics.off()

#library(EMD)
library(coda)
library(mcmc)
library(batchmeans)
library(spgs)
library(ggplot2)
library(bayesplot)
library(RColorBrewer)
library(gridExtra)
library(RColorBrewer)


textrm1<-as.matrix(read.table("HCN_data/USC00365915.dly",header=FALSE))
row<-dim(textrm1)[1]
col<-dim(textrm1)[2]
textrm1<-textrm1[ ,2:col]
col<-col-1
class(textrm1)<-"numeric"
textrm1<-textrm1/10   #in celcius
textrm1_edd<-textrm1
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm1[i,j])==FALSE){
      if (textrm1[i,j] < 10){
        textrm1[i,j] <- 10
      }
      if (textrm1[i,j] > 29){
        textrm1[i,j] <- 29
      }
    }
  }
}
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm1_edd[i,j])==FALSE){
      if (textrm1_edd[i,j] < 29){
        textrm1_edd[i,j] <- 29
      }
    }
  }
}

GDD1<-rep(0,28)
for (i in 1:16){  #GDD1 in each year
  GDD1[i]<-sum(textrm1[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184 #184 days per year
}
i<-17
GDD1[17]<-(sum(textrm1[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-10*(184-31))*184/153 #missing march
for (i in 18:28){ 
  GDD1[i]<-sum(textrm1[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-10*184
}
EDD1<-rep(0,28)
for (i in 1:16){  #EDD1 in each year
  EDD1[i]<-sum(textrm1_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184 #184 days per year
}
i<-17
EDD1[17]<-(sum(textrm1_edd[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-29*(184-31))*184/153 #missing march
for (i in 18:28){ 
  EDD1[i]<-sum(textrm1_edd[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-29*184
}


textrm2<-as.matrix(read.table("HCN_data/USC00367322.dly",header=FALSE))
row<-dim(textrm2)[1]
col<-dim(textrm2)[2]
textrm2<-textrm2[ ,2:col]
col<-col-1
class(textrm2)<-"numeric"
textrm2<-textrm2/10   #in celcius
textrm2_edd<-textrm2
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm2[i,j])==FALSE){
      if (textrm2[i,j] < 10){
        textrm2[i,j] <- 10
      }
      if (textrm2[i,j] > 29){
        textrm2[i,j] <- 29
      }
    }
  }
}
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm2_edd[i,j])==FALSE){
      if (textrm2_edd[i,j] < 29){
        textrm2_edd[i,j] <- 29
      }
    }
  }
}

GDD2<-rep(0,28)
for (i in 1:7){  #GDD2 in each year
  GDD2[i]<-sum(textrm2[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184
}
i<-8
GDD2[8]<-(sum(textrm2[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*(184-14))*184/170 #missing half April
for (i in 9:28){  
  GDD2[i]<-sum(textrm2[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184
}
EDD2<-rep(0,28)
for (i in 1:7){  #EDD2 in each year
  EDD2[i]<-sum(textrm2_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184
}
i<-8
EDD2[8]<-(sum(textrm2_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*(184-14))*184/170 #missing half April
for (i in 9:28){  
  EDD2[i]<-sum(textrm2_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184
}


textrm3<-as.matrix(read.table("HCN_data/USC00368449.dly",header=FALSE))
row<-dim(textrm3)[1]
col<-dim(textrm3)[2]
textrm3<-textrm3[ ,2:col]
col<-col-1
class(textrm3)<-"numeric"
textrm3<-textrm3/10   #in celcius
textrm3_edd<-textrm3
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm3[i,j])==FALSE){
      if (textrm3[i,j] < 10){
        textrm3[i,j] <- 10
      }
      if (textrm3[i,j] > 29){
        textrm3[i,j] <- 29
      }
    }
  }
}
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm3_edd[i,j])==FALSE){
      if (textrm3_edd[i,j] < 29){
        textrm3_edd[i,j] <- 29
      }
    }
  }
}

GDD3<-rep(0,28)
for (i in 1:20){  #GDD3 in each year
  GDD3[i]<-sum(textrm3[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184
}
i<-21
GDD3[21]<-(sum(textrm3[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-10*(184-31))*184/153 #missing 2000 May
for (i in 22:28){ 
  GDD3[i]<-sum(textrm3[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-10*184
}
EDD3<-rep(0,28)
for (i in 1:20){  #EDD3 in each year
  EDD3[i]<-sum(textrm3_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184
}
i<-21
EDD3[21]<-(sum(textrm3_edd[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-29*(184-31))*184/153 #missing 2000 May
for (i in 22:28){ 
  EDD3[i]<-sum(textrm3_edd[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-29*184
}


textrm4<-as.matrix(read.table("HCN_data/USC00369050.dly",header=FALSE))
row<-dim(textrm4)[1]
col<-dim(textrm4)[2]
textrm4<-textrm4[ ,2:col]
col<-col-1
class(textrm4)<-"numeric"
textrm4<-textrm4/10   #in celcius
textrm4_edd<-textrm4
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm4[i,j])==FALSE){
      if (textrm4[i,j] < 10){
        textrm4[i,j] <- 10
      }
      if (textrm4[i,j] > 29){
        textrm4[i,j] <- 29
      }
    }
  }
}
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm4_edd[i,j])==FALSE){
      if (textrm4_edd[i,j] < 29){
        textrm4_edd[i,j] <- 29
      }
    }
  }
}
GDD4<-rep(0,28)
i<-1
GDD4[1]<-(sum(textrm4[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*(184-29))*184/155 #missing 1980 Apr
for (i in 2:14){  #GDD4 in each year
  GDD4[i]<-sum(textrm4[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184
}
i<-15
GDD4[15]<-(sum(textrm4[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-10*(184-30))*184/154 #missing 1994 Apr, 1999 Apr
for (i in 16:19){ 
  GDD4[i]<-sum(textrm4[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-10*184
}
i<-20
GDD4[20]<-(sum(textrm4[(12*(i-1)-1):(12*i-4), ], na.rm=TRUE)/2-10*(184-30))*184/154
for (i in 21:28){ 
  GDD4[i]<-sum(textrm4[(12*(i-1)-3):(12*i-4), ], na.rm=TRUE)/2-10*184
}
EDD4<-rep(0,28)
i<-1
EDD4[1]<-(sum(textrm4_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*(184-29))*184/155 #missing 1980 Apr
for (i in 2:14){  #EDD4 in each year
  EDD4[i]<-sum(textrm4_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184
}
i<-15
EDD4[15]<-(sum(textrm4_edd[(12*(i-1)+1):(12*i-2), ], na.rm=TRUE)/2-29*(184-30))*184/154 #missing 1994 Apr, 1999 Apr
for (i in 16:19){ 
  EDD4[i]<-sum(textrm4_edd[(12*(i-1)-1):(12*i-2), ], na.rm=TRUE)/2-29*184
}
i<-20
EDD4[20]<-(sum(textrm4_edd[(12*(i-1)-1):(12*i-4), ], na.rm=TRUE)/2-29*(184-30))*184/154
for (i in 21:28){ 
  EDD4[i]<-sum(textrm4_edd[(12*(i-1)-3):(12*i-4), ], na.rm=TRUE)/2-29*184
}

textrm5<-as.matrix(read.table("HCN_data/USC00369298.dly",header=FALSE))
row<-dim(textrm5)[1]
col<-dim(textrm5)[2]
textrm5<-textrm5[ ,2:col]
col<-col-1
class(textrm5)<-"numeric"
textrm5<-textrm5/10   #in celcius
textrm5_edd<-textrm5
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm5[i,j])==FALSE){
      if (textrm5[i,j] < 10){
        textrm5[i,j] <- 10
      }
      if (textrm5[i,j] > 29){
        textrm5[i,j] <- 29
      }
    }
  }
}
for (i in 1:row){
  for (j in 1:col){
    if (is.na(textrm5_edd[i,j])==FALSE){
      if (textrm5_edd[i,j] < 29){
        textrm5_edd[i,j] <- 29
      }
    }
  }
}

GDD5<-rep(0,28)
for (i in 1:28){  #GDD5 in each year
  GDD5[i]<-sum(textrm5[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-10*184
}
EDD5<-rep(0,28)
for (i in 1:28){  #EDD5 in each year
  EDD5[i]<-sum(textrm5_edd[(12*(i-1)+1):(12*i), ], na.rm=TRUE)/2-29*184
}



yield<-as.matrix(read.table("yield_data/yield.csv",header=FALSE,sep=",",skip = 1))
entireG<-rep(0,140) #growing degree days in each year, each station, unit in degree
entireG[1:28]<-GDD1
entireG[29:56]<-GDD2
entireG[57:84]<-GDD3
entireG[85:112]<-GDD4
entireG[113:140]<-GDD5
entireE<-rep(0,140) #extreme degree days in each year, each station, unit in degree
entireE[1:28]<-EDD1
entireE[29:56]<-EDD2
entireE[57:84]<-EDD3
entireE[85:112]<-EDD4
entireE[113:140]<-EDD5
entirey<-rep(0,140)  #crop yields in each year, each station
entirey[1:28]<-rev(yield[ ,1])
entirey[29:56]<-rev(yield[ ,2])
entirey[57:84]<-rev(yield[ ,3])
entirey[85:112]<-rev(yield[ ,4])
entirey[113:140]<-rev(yield[ ,5])

yield_loess1=predict(loess(entirey[1:28]~c(1:28),span=0.75))
yield_loess2=predict(loess(entirey[29:56]~c(1:28),span=0.75))
yield_loess3=predict(loess(entirey[57:84]~c(1:28),span=0.75))
yield_loess4=predict(loess(entirey[85:112]~c(1:28),span=0.75))
#yield_loess5=predict(loess(entirey[1:28]~c(113:140),span=0.8)) there are 2 missing values
plot(c(1980:2007),entirey[1:28],type="p",pch=20,xlab = "Year",ylab="Corn yield (bu/acre)"
     ,main = "Annual corn yield at Montrose station from 1980 to 2007")
lines(c(1980:2007),yield_loess1,col="red")
points(c(1980:2007),yield_loess1,pch=20,col="red")
legend(1978.9,127.3,pch=c(20,20), legend=c("Original yield","Loess regression trend"),col = c("black","red"))


yield_anomaly<-rep(0,112) #make all data in anomaly space
yield_anomaly[1:28]<-entirey[1:28]-yield_loess1
yield_anomaly[29:56]<-entirey[29:56]-yield_loess2
yield_anomaly[57:84]<-entirey[57:84]-yield_loess3
yield_anomaly[85:112]<-entirey[85:112]-yield_loess4
GDD_anomaly<-rep(0,112)
GDD_anomaly<-entireG[1:112]-mean(entireG[1:112],na.rm = TRUE)
EDD_anomaly<-rep(0,112)
EDD_anomaly<-entireE[1:112]-mean(entireE[1:112],na.rm = TRUE)

#plot(entireG[1:28],yield_anomaly[1:28],col="red",pch=20,xlim = c(750,1350),ylim = c(-60,40),
#     xlab = "Effective growing degree days (??C)",ylab = "Corn yield anomaly (Bu/acre)",
#     main = "Regression of corn yield anomaly to growing degree days from 1980 to 2007 in 4 PA stations")
#points(entireG[29:56],yield_anomaly[29:56],col="orange",pch=20)
#points(entireG[57:84],yield_anomaly[57:84],col="blue",pch=20)
#points(entireG[85:112],yield_anomaly[85:112],col="green",pch=20)
#gdd1<-entireG[1:112]
#linearmod<-lm(yield_anomaly[1:112] ~ gdd1)
#lines(seq(730,1350,5),-73.721376+0.07094*seq(730,1350,5),lwd=3)
#legend(1091,-41,pch = c(20,20,20,20,NA),lwd=c(NA,NA,NA,NA,3),legend = c("Montrose","Reading","State College","Uniontown","Linear regression"),
#       col=c("red","orange","blue","green","black"),ncol = 2,cex=0.8)
#plot(entireG[1:112],yield_anomaly,pch=20)


#variables in model include GDD, GDD^2, EDD, EDD^2

GDD_sqr<-GDD_anomaly^2
EDD_sqr<-EDD_anomaly^2
GDD_cub<-GDD_anomaly^3
EDD_cub<-EDD_anomaly^3


#step(lm(yield_anomaly ~ 1),yield_anomaly ~ GDD_anomaly+GDD_sqr+GDD_cub+EDD_anomaly+EDD_sqr+EDD_cub,direction = "forward")
yielddata<-matrix(0,nrow = 112,ncol = 7)
yielddata[ ,1]<-yield_anomaly
yielddata[ ,2]<-GDD_anomaly
yielddata[ ,3]<-GDD_sqr
yielddata[ ,4]<-GDD_cub
yielddata[ ,5]<-EDD_anomaly
yielddata[ ,6]<-EDD_sqr
yielddata[ ,7]<-EDD_cub
yielddata<-data.frame(yielddata)
colnames(yielddata)<-c("yield","GDD","GDD_sqr","GDD_cub","EDD","EDD_sqr","EDD_cub")
#confint(linearmod,level=0.95) #confident interval from the model
#vcov(linearmod)

#cross-validation method, split the data into 10 groups
c<-c(1:112)
groups<-split(c,sample(rep(1:10,each = 11)))
CV<-rep(0,10)
for (i in 1:10) {  #10 cross-validation tests
  test_indx<-groups[[i]]
  test_data<-yield_anomaly[test_indx]
  train_indx<-c[-test_indx]
  train_model<-lm(yield ~ EDD+EDD_sqr,data = data.frame(yielddata[train_indx, ])) #training model
  pred<-predict(train_model,data.frame(EDD=yielddata[test_indx,5],EDD_sqr=yielddata[test_indx,6])) #which variables to test?
  CV[i]<-mean((pred-test_data)^2)
}

#bootstrap strategy: take 100 sample each time. repeat 1000 times
A<-rep(0,1000)
B<-rep(0,1000)
C<-rep(0,1000)
D<-rep(0,1000)
E<-rep(0,1000)
for (i in 1:1000) {
  indx<-sample(c,100,replace = FALSE)
  linearmod<-lm(yield ~ GDD + GDD_sqr + EDD + EDD_sqr,data = data.frame(yielddata[indx, ]))
  E[i]<-linearmod$coefficients[1]
  A[i]<-linearmod$coefficients[2]
  B[i]<-linearmod$coefficients[3]
  C[i]<-linearmod$coefficients[4]
  D[i]<-linearmod$coefficients[5]
}
plot(density(A),xlab = "coefficient of GDD",ylab = "density",
     main = "density plof of bootstraps of GDD coefficient")
quantile(A,c(0.025,0.975)) #confidence interval from bootstrap



#MCMC
#data to use are yield~GDD+EDD+EDD_sqr
model <- function(parm,w,x,y,z){ # Inputs are parameters and length of data
  model.p <- length(parm) # number of parameters in the physical model
  gdd <- parm[1]
  gdd_sqr<-parm[2]
  edd <- parm[3]
  edd_sqr <- parm[4]
  ymean <- parm[5]
  y.mod <- gdd*w + gdd_sqr*x + edd*y + edd_sqr*z + ymean # This linear equation represents the physical model
  return(list(mod.obs = y.mod, model.p = model.p))
}

observations <- yielddata[ ,1]
parnames<-c("GDD","GDD_sqr","EDD","EDD_sqr","yield mean","sigma")
set.seed(128)
#initial guess GDD, EDD, EDD^2, yield mean, sigma
p0<-c(0.04,0,-0.35,-0.003,2,1)
p<-c(0.04,0,-0.25,-0.004,1.5,0.8)
# Load the likelihood model for measurement errors
source("my_iid_obs_likelihood.R")
step <- c(0.004, 0.0001, 0.04, 0.00022, 0.6, 0.4)
NI <- 100000
model.p<-5
bound.lower<-c(-10,-5,-10,-5,-10,0)
bound.upper<-c(10, 5, 5, 5, 10,100)
mcmc.out <- metrop(log.post, p0, nbatch = NI, scale = step)
prechain <- mcmc.out$batch
# Print the acceptance rate as a percent.
acceptrate <- mcmc.out$accept * 100
cat("Accept rate =", acceptrate, "%\n")
# Identify the burn-in period and subtract it from the chains.
burnin <- seq(1, 0.01*NI, 1)
mcmc.chains <- prechain[-burnin, ]
par(mfrow = c(2,3))
for(i in 1:6){
  plot(mcmc.chains[ ,i], type="l", main = "",
       ylab = paste('Parameter = ', parnames[i], sep = ''), xlab = "Number of Runs")
}


bm_est <- bmmat(mcmc.chains)
print(bm_est)
z <-
  half_width <- rep(NA, length(parnames))
interval <- matrix(data = NA, nrow = 6, ncol = 2,
                   dimnames = list(c(1:6), c("lower_bound", "upper_bound")))
for(i in 1:length(parnames)){
  z[i] <- (mean(mcmc.chains[,i]) - bm_est[i ,"est"])/bm_est[i ,"se"]
  half_width[i] <- z[i] * bm_est[i ,"se"]
  interval[i,1] <- bm_est[i ,"est"] - half_width[i]
  interval[i,2] <- bm_est[i ,"est"] + half_width[i]
}
print(interval)

heidel.diag(mcmc.chains, eps = 0.1, pvalue = 0.05)

## Check #4: Gelman and Rubin's convergence diagnostic:
set.seed(100)
p0 <- c(0.03,0,-0.31,-0.0025,3,1.1) # Arbitrary choice.
mcmc.out2 <- metrop(log.post, p0, nbatch=NI, scale=step)
prechain2 <- mcmc.out2$batch
set.seed(158)
p0 <- c(0.033,0,-0.37,-0.0033,2.5,0.4) # Arbitrary choice.
mcmc.out3 <- metrop(log.post, p0, nbatch=NI, scale=step)
prechain3 <- mcmc.out3$batch
set.seed(123)
p0 <- c(0.042,0,-0.33,-0.0028,1.8,0.7) # Arbitrary choice.
mcmc.out4 <- metrop(log.post, p0, nbatch=NI, scale=step)
prechain4 <- mcmc.out4$batch
# The burn-in has already been subtracted from the first chain.
# Thus, the burn-in only needs to be subtracted from the three other
# chains at this point.
mcmc1 <- as.mcmc(mcmc.chains)
mcmc2 <- as.mcmc(prechain2[-burnin, ])
mcmc3 <- as.mcmc(prechain3[-burnin, ])
mcmc4 <- as.mcmc(prechain4[-burnin, ])
set.seed(1) # revert back to original seed
mcmc_chain_list <- mcmc.list(list(mcmc1, mcmc2, mcmc3, mcmc4))
gelman.diag(mcmc_chain_list)
gelman.plot(mcmc_chain_list)


# Calculate the 90% highest posterior density CI.
# HPDinterval() requires an mcmc object; this was done in the code block above.
mcmc1 <- as.mcmc(mcmc.chains)
hpdi = HPDinterval(mcmc1, prob = 0.90)
# Create density plot of each parameter.
i=6
  # Create density plot.
  p.dens = density(mcmc.chains[,i])
  layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
  par(mar=c(0, 5.1, 1.1, 2.1))
  boxplot(mcmc.chains[ ,i] , horizontal=TRUE , xaxt="n", frame=F,pch=20)
  par(mar=c(4, 5.1, 1.1, 2.1))
  plot(p.dens, xlab = paste('Parameter =',' ', parnames[i], sep = ''), main="")
  # Add mean estimate.
  abline(v = bm(mcmc.chains[,i])$est, lwd = 2,col="blue")
  # Add 90% equal-tail CI.
  CI = quantile(mcmc.chains[,i], prob = c(0.05, 0.95))
  lines(x = CI, y = rep(0, 2), lwd = 2,col="violet")
  points(x = CI, y = rep(0, 2), pch = 16,col="violet")
  # Add 90% highest posterior density CI.
  lines(x = hpdi[i, ], y = rep(mean(p.dens$y), 2), lwd = 2, col = "red")
  points(x = hpdi[i, ], y = rep(mean(p.dens$y), 2), pch = 16, col = "red")
legend("topleft",lwd = c(1,2,2,2),col=c("black","violet","red","blue"),legend = c("posterior distribution","90% equal-tail CI","90% highest posterior density CI","mean estimation"))

true_model<-bm_est[1, 1]*yielddata[ ,2]+bm_est[2,1]*yielddata[ ,3]+bm_est[3,1]*yielddata[ ,5]+bm_est[4,1]*yielddata[6]+bm_est[5,1]
res<-yielddata[ ,1]-true_model
par(mfrow = c(1,1))
plot(density(res[ ,1]),xlab = "residual between model and observation",ylab = "density",main = "Residual distribution")
x<-seq(-40,40,length=801)
y<-dnorm(x,0.08185,14.45153)
lines(x,y,col="red")
shapiro.test(res[ ,1])

#colnames(mcmc.chains)<-parnames
#color_scheme_set("red")
#p2 <- mcmc_scatter(mcmc.chains, pars = c("GDD", "EDD"), pch=".",size = 3.5, alpha = 0.25)
#p2 + stat_density_2d(color = "black", size = .5)

#my.cols <- rev(brewer.pal(11, "RdBu"))
#z <- kde2d(mcmc.chains[ ,1], mcmc.chains[ ,3], n=50) #which 2 variables?
#plot(mcmc.chains[ ,1],mcmc.chains[ ,3], xlab="GDD", ylab="EDD", pch="20",cex=0.2, col="gray")
#contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, lwd=3, add=TRUE)

modelyield<-matrix(0, nrow = 112, ncol = 10000)
for (i in 1:112){
     for (j in 1:10000){
         modelyield[i,j]<-model(c(mcmc1[j+10000,1],mcmc1[j+10000,2],mcmc1[j+10000,3],mcmc1[j+10000,4],mcmc1[j+10000,5]),yielddata[i,2],yielddata[i,3],yielddata[i,5],yielddata[i,6])$mod.obs+rnorm(1,0,mcmc1[j+10000,6])
     }
}
boundyield<-matrix(0,nrow = 112,ncol = 2)
for (i in 1:112){
     a<-quantile(modelyield[i, ],probs = c(0.05,0.95))
    boundyield[i,1]<-a[1]
    boundyield[i,2]<-a[2]
}
plot(c(1980:2007),yield_anomaly[1:28],pch=20,xlab = "year",ylab = "yield (bu/acre)")
lines(c(1980:2007),boundyield[1:28,1])
lines(c(1980:2007),boundyield[1:28,2])