## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ZooScatR)
library(reshape2)
library(ggplot2)

## ----settings------------------------------------------------------------
fname <- paste0(system.file(package="ZooScatR"),"/extdata/configs/config_0.dat") #Loacation of the parameters file
para = read_para(fname) #Read parameters file
para$simu$var0 = 18 #simulate from 10 
para$simu$var1 = 300 #...to 300 kHz
para$simu$ni = 200 #resudce the number of elements and frequencies to improve speed
para$simu$n = 283

#Create list with soundspeed info
misc <- list(cw=1500)

#change to the more realistic krill shape
profname <- paste0(system.file(package="ZooScatR"),"/extdata/profiles/euphaus1.dat") #krill example
para$shape$prof_name <- profname

#make a length simulation
para$shape$ave_flag = 1

#make an orientation simulation
para$orient$ave_flag = 1

## ----simulation----------------------------------------------------------
#Run DWBA based on config file
res <- bscat(para=para, misc=misc, simOut = TRUE, nang=100, nl=100) #Target strength vs Frequency

## ----plots_osim----------------------------------------------------------
o_sim = melt(res$ysim)
o_sim$theta = res$ang[o_sim$Var2]
o_sim$Frequency = res$var[o_sim$Var1]
o_sim=o_sim[,3:ncol(o_sim)]
names(o_sim)[1] <- 'TS'

mTS = data.frame(Frequency=res$var,TS=res$y)

ggplot()+geom_line(data=o_sim, aes(x = Frequency, y=TS, group=theta),lty=2, lwd=0.5, alpha=0.4)+
  geom_line(data=mTS, aes(x=Frequency, y=TS), lty=1, lwd=2)+theme_classic()+theme(text=element_text(size=14))

ggplot(data=o_sim, aes(x=Frequency, y=theta, fill=TS))+
  geom_raster()+
  scale_fill_viridis_c()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  theme_classic()+theme(text=element_text(size=14))

## ----plots_L-------------------------------------------------------------
l_sim =  melt(res$ysimL)
l_sim$Length = res$L[l_sim$Var2]
l_sim$Frequency = res$var[l_sim$Var1]
l_sim=l_sim[,3:ncol(l_sim)]
names(l_sim)[1] <- 'TS'

ggplot()+geom_line(data=l_sim, aes(x = Frequency, y=TS, group=Length),lty=2, lwd=0.5, alpha=0.4)+
  geom_line(data=mTS, aes(x=Frequency, y=TS), lty=1, lwd=2)+theme_classic()+theme(text=element_text(size=14))



ggplot(data=l_sim, aes(x=Frequency, y=Length, fill=TS))+
  geom_raster()+
  scale_fill_viridis_c()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  theme_classic()+theme(text=element_text(size=14))


