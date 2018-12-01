#' Read the configuration file
#' Keep standard dat format, as used in Matlab, to keep maximum compatibility with previous files
#' @import ggplot2
#' @import pracma
#' @param x x coordinates
#' @param y y corrdinates
#' @param z z coordinates
#' @return shape
#' @examples
#' sub <- shapes[which(shapes$ID==unique(shapes$ID)[x]),]
#' generate_pos(sub$x,sub$y,sub$z)
#' @export

generate_pos <- function(x=sub$x,y=sub$y,z=sub$z){
  x=x-min(x)
  y=y-min(y)
  z=z-min(z)
  y2=0
  dist3 <- function(x1,y1,z1,x2,y2,z2){sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2)}
  dist2 <- function(x1,y1,x2,y2){sqrt((x1-x2)^2+(y1-y2)^2)}
  for(i in 1:(length(x))){
    for(j in c(2:length(x),1)){
      d3=dist3(x[i],y[i],z[i],x[j],y[j],z[j])
      d2=dist2(x[i],y[i],x[j],y[j])
      #y2[i]=y[i]+d3-d2
      y2[i]=d3
    }
  }

  ddd<-as.data.frame(cbind(x=x,y=y,y2=y2))
  print(ggplot2::ggplot(data=ddd)+
    ggplot2::geom_polygon(ggplot2::aes(x=x,y=y), alpha=0.9, fill="gray", col="black", lty=2)+
    ggplot2::geom_polygon(ggplot2::aes(x=x,y=z), alpha=0.7, fill="lightgray", col="black", lty=3)+
    ggplot2::geom_polygon(ggplot2::aes(x=x,y=y2), fill="lightblue", alpha=0.8, col="black")+
    ggplot2::coord_fixed())
  print(ggplot(data=ddd)+
    geom_polygon(aes(x=x,y=y), alpha=0.9, fill="gray", col="black", lty=2)+
    geom_polygon(aes(x=x,y=z), alpha=0.7, fill="lightgray", col="black", lty=3)+
    geom_polygon(aes(x=x,y=y2), fill="lightblue", alpha=0.8, col="black")+
    coord_fixed())

  n=200
  x_b=x[1:max(which(x==max(x)))]
  y_b=y2[1:max(which(x==max(x)))]
  x_t=x[max(which(x==max(x))):length(x)]
  y_t=y2[max(which(x==max(x))):length(y)]

  top <- pracma::interp1(x_t,y_t,seq(min(x), max(x), length=n), method="linear")
  bottom <- (pracma::interp1(x_b,y_b,seq(min(x), max(x),length=n), method="linear"))
  top <- interp1(x_t,y_t,seq(min(x), max(x), length=n), method="linear")
  bottom <- (interp1(x_b,y_b,seq(min(x), max(x),length=n), method="linear"))
  mid <- as.data.frame(cbind(x=seq(min(x), max(x), length=n),
                             y=rowMeans(cbind(top,bottom))))
  mid$taper=sqrt((top-bottom)^2)

  print(ggplot2::ggplot()+
    ggplot2::geom_polygon(data=as.data.frame(cbind(x=x,y=y2)), ggplot2::aes(x=x,y=y2), alpha=0.5)+
    ggplot2::geom_line(data=mid, ggplot2::aes(x=x,y=y)))
  print(ggplot()+
    geom_polygon(data=as.data.frame(cbind(x=x,y=y2)), aes(x=x,y=y2), alpha=0.5)+
    geom_line(data=mid, aes(x=x,y=y)))
  mid$p=0
  return(mid[,c(x=2,z=1,taper=3, p=4,p2=4)])
}

#
#
# # setwd("/Users/sveng/ZooScatR/ZooScatR/")
# # source("DWBAbscat.R")
# # source("smoother.R")
# # source("read_para.R")
# # source("orient_ave.R")
# # source("length_ave.R")
# # source("buildpos.R")
# # source("bscat.R")
# # source("createfile.R")
# # source("create_profile.R")
# # source("get_parameters.R")
# # source("inhom_gh.R")
# # source("save_config.R")
# # source("smoother.R")
# #
# # #Video output
# path <- "/Users/sveng/Documents/echoview/"
# shapes <- read.csv(paste0(path,list.files(path=path,pattern="krill*")))
#
# shapes$ID <- paste0("F",shapes$Frame,"C",shapes$Cont)
#
# x=1200
# px2mm <- 19/23
# sub <- shapes[which(shapes$ID==unique(shapes$ID)[x]),]
# sub$x<-sub$x*px2mm;sub$z<-sub$z*px2mm;sub$y<-sub$y*px2mm;
# pos120<-generate_pos(sub$x,sub$y,sub$z)
#
# L=dist2(pos120$x[1],pos120$y[1],pos120$x[length(pos120$x)],pos120$y[length(pos120$y)])
#
# profnam=paste0("pos",x,".dat")
#
# write.table(pos120,file=profnam,sep="\t", col.names = FALSE, row.names = FALSE)
# #read.table(profnam)
# #Set filename ro config file
# fname = "/Users/sveng/Documents/Tank/GUI_DWBA_scat_model/mfiles/configuation/config_0.dat"
# #read config file
# para = read_para(fname)
# #Create list with soundspeed info
# misc <- list()
# misc$cw <- 1460
# #Create status list
# status<- list()
# status$stop = 0
#
# para$shape$prof_name = paste0(getwd(),'/',profnam)
#
# #set sampling
# para$simu$n <- length(seq(90,170))
# para$simu$var0 <- 90
# para$simu$var1 <- 170
#
# para$shape$L <- L
# para$shape$ave_flag <- -1
#
# para$shape$axis_sm<-0
# para$shape$taper_sm<-0
#
# para$orient$ave_flag <- -1
# para$orient$angm <- -90
#
#
# #Run DWBA based on config file
# res <- bscat(para=para, misc=misc, status=status)
# res$rplot
#
#
# for(x in sample(1:length(unique(shapes$ID)),20)){
# sub <- shapes[which(shapes$ID==unique(shapes$ID)[x]),]
# generate_pos(sub$x,sub$y,sub$z)
# }
