## ----warning=FALSE-------------------------------------------------------
library(ZooScat)
library(ggplot2)
library(viridis)

## ------------------------------------------------------------------------
r <- 10 #range 
a <- 0.01 # radius
c <- 1477.4 #soundspeed surrounding fluid
rho <- 1026.8 #density surrounding fluid
g <- 1028.9/rho #density contrast
h <- 1480.3/c #soundspeed contrast
f <- 200 * 1000 #Frequency in Hz
TS.sphere2(f=f,r=r,a=a,c=c,h=h,g=g,rho=rho)

## ------------------------------------------------------------------------
r <- 10 #range 
a <- 0.01 # radius
c <- 1477.4 #soundspeed surrounding fluid
rho <- 1026.8 #density surrounding fluid
g <- 1028.9/rho #density contrast
h <- 1480.3/c #soundspeed contrast

#Frequency range
fmin=12
fmax=400
freqs <- seq(fmin,fmax, by=0.5)

## ------------------------------------------------------------------------
fname <- paste0(system.file(package="ZooScat"),"/extdata/configs/config_0.dat") #Loacation of the parameters file
para = read_para(fname) #Read parameters file

#Create list with soundspeed info
misc <- list(cw=c) #Set soundspeed in the surrounding fluid

#Set the material properties
para$phy$g0 <- g #set the density contrast
para$phy$h0 <- h #set the soundspeed contrast

#set the simulation parameters
para$simu$var0<-fmin #Set the minimum frequency
para$simu$var1<-fmax #Set the maximum frequency
para$simu$n=length(freqs) #Set the number of output points
para$simu$ni=2000 #any high number


## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
#Set the shape parameters
para$shape$L <- 2*1000*a # 2 * the radius in m to get length in mm
para$shape$order <- 2 #Set the tapering order
para$shape$L_a <- 2 #Set L/a
para$shape$rho_L <- 2000 #any high number to get a high curvature

#Build the sphere and plot the result
sp <- buildpos(para) #build the shape
sp$plot #show the shape plot

## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
fs <- as.list(freqs*1000) #Create a list of frequencies
TS.MJ <- sapply(fs,TS.sphere2,r=r,a=a,c=c,h=h,g=g,rho=rho) #Apply frequency list to the analytical model

## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
#Run DWBA based on config file
DWBA <- bscat(para=para, misc=misc)
#Show the model outcome
DWBA$rplot


## ----warning=FALSE,fig.width=8,fig.height=4,tidy=TRUE--------------------
#Compare results
TS.comp<-function(TS1,TS2,freqs){
  require(ggplot2)
  require(viridis)
  TScomp <- as.data.frame(cbind(TS = c(TS1,TS2),
                 Freq = rep(freqs,2),
                 Model=rep(c("Analytical","DWBA"),each=length(freqs))))
  names(TScomp)<- c("TS","Frequency","Model")
  TScomp$TS <- as.numeric(as.character(TScomp$TS))
  TScomp$Frequency <- as.numeric(as.character(TScomp$Frequency))
  
  err <- sum(abs(TS1-TS2))/length(freqs)
  TScomp$diff <- rep(abs(TS1-TS2),2)
  message(paste("Overall error:", round(mean(err),2),"dB"))
  #ggplot remove backgorund
  rmb <- theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())
  
  print(ggplot(data=TScomp,aes(x=Frequency,y=TS, group=Model,col=Model))+
          geom_line(lwd=1)+
          scale_color_brewer(palette="Set1")+
          theme(legend.position = "top")+
          xlab("Frequency [kHz]")+
          rmb)
  print(ggplot(data=TScomp,aes(x=Frequency,y=diff, col=diff))+
          geom_line(lwd=1.2)+
          scale_color_viridis(name="dB difference")+
          theme(legend.position="top")+xlab("Frequency [kHz]")+rmb)
  return(TScomp)
}

## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
TS.compare <- TS.comp(TS.MJ,DWBA$y,freqs)

## ------------------------------------------------------------------------
a <- 0.01
g <- 1.0357 #density contrast
h <- 1.0279 #soundspeed contrast
c <- 1456

#Set the material properties in ZooScat
para$shape$L <- 2*1000*a # 2 * the radius in m to get length in mm
para$phy$g0 <- g #set the density contrast
para$phy$h0 <- h #set the soundspeed contrast
misc$cw <- c

#Run the analytical model
TS.MJ <- sapply(fs,TS.sphere2,r=r,a=a,c=c,h=h,g=g,rho=rho) #Apply frequency list to the analytical model

#Run the ZooScat DWBA:  
DWBA <- bscat(para=para, misc=misc)

## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
TS.compare <- TS.comp(TS.MJ,DWBA$y,freqs)

## ------------------------------------------------------------------------
a <- 0.0381
g <- 14900/rho #density contrast
h <- 6853/c #soundspeed contrast

#Set the material properties in ZooScat
para$shape$L <- 2*1000*a # 2 * the radius in m to get length in mm
para$phy$g0 <- g #set the density contrast
para$phy$h0 <- h #set the soundspeed contrast

#Run the analytical model
TS.MJ <- sapply(fs,TS.sphere2,r=r,a=a,c=c,h=h,g=g,rho=rho) #Apply frequency list to the analytical model

#Run the ZooScat DWBA:  
DWBA <- bscat(para=para, misc=misc)

## ----warning=FALSE,fig.width=8,fig.height=4------------------------------
TS.compare <- TS.comp(TS.MJ,DWBA$y,freqs)

