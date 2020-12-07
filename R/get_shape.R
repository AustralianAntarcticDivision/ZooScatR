#' Get polygon points from contour coordinates
#' @param xyP contour points with columns x, y
#' @param lout length of the output data frame
#' @import sf
#'
get_int<- function(xyP,lout=1000){
  poly = st_polygon(list(as.matrix(xyP[,c('x','y')])))
  pts = {}
  for(x in seq(min(xyP$x),max(xyP$x),length.out = lout)){
    ll = st_linestring(rbind(c(x,min(xyP$y)),c(x,max(xyP$y))))
    inter = st_intersection(ll,poly)
    if(length(inter) > 0){
      ys = st_coordinates(st_intersection(ll,poly))[,2]
      pts = rbind(pts, rbind(c(x,min(ys)), c(x,max(ys))))
    }
  }
  pts[,2] = abs(pts[,2]-max(pts[,2]))
  xy=data.frame(pts)
  names(xy)=c('x','y')
  return(xy)
}

#' @title Rotate axis
#' @param xy dataframe with xy coordinates
#' @param angle rotation angle
rotate <- function (xy, angle) {
  cos.angle <- cos(angle)
  sin.angle <- sin(angle)
  xy.rot <- as.matrix(xy) %*% t(matrix(c(cos.angle, sin.angle, -sin.angle,
                                         cos.angle), 2, 2))
  xy = data.frame(xy.rot)
  names(xy) = c('x','y')
  return(xy)
}

#' @title Densify points along axis
#' @param xy dataframe with xy coordinates
#' @param b densify factor
densify <- function(xy,n=20){
  ## densify a 2-col matrix
  cbind(dens(xy[,1],n=n),dens(xy[,2],n=n))
}

#' @title Densify a vector
#' @param x vector to be densified
#' @param n densification factor
#'
dens <- function(x,n=15){
  ## densify a vector
  out = rep(NA,1+(length(x)-1)*(n+1))
  ss = seq(1,length(out),by=(n+1))
  out[ss]=x
  for(s in 1:(length(x)-1)){
    out[(1+ss[s]):(ss[s+1]-1)]=seq(x[s],x[s+1],len=(n+2))[-c(1,n+2)]
  }
  out
}
#' @title Generate a ZooScatR shape file from x,y contour points
#' @description Generates a shape file tha tcan be  used directly in a ZooScatR model, based on contour coordinates
#' @param fn character, input filename containing 2 columns with the x, y coordinates
#' @param outdir character, folder to which the the output shape should be saved, defaults to '', which selectds the current working directory
#' @param ndens integer, number of points that will be used as a densification factor, defaults 500
#' @prec deimal points precision at which duplicated point will be averaged, defaults to 2
#' @param rot boolean, TRUE/FALSE, used if x and y columns in the input file are flipped
#' @param res numeric, defines the output resolution of the shape, defaults 0.02
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
#'
get_mid <- function(fn, outdir='', ndens=500, prec=2, rot=FALSE,res=0.02){
  xyP = read.table(fn, header = FALSE)
  if (rot==TRUE){
    names(xyP) <- c('x','y')
  }else{
    names(xyP) <- c('y','x')
  }
  xyP$x = as.numeric(xyP$x)
  xyP$y = as.numeric(xyP$y)
  xyP = rbind(xyP,xyP[1,])

  xyP = xyP[,c('x','y')]

  xyP$y = -xyP$y

  xy = get_int(xyP)

  #only keep duplicates
  xy = xy[xy$x %in% xy$x[duplicated(xy$x)],]

  #only unique pairs
  xy <- unique( xy[ , 1:2 ] )
  xy = round(xy,prec)

  mm = xy%>%group_by(x=round(x,2))%>%summarize(y=median(y))

  mpx = length(mm$x)/2

  lmx = lm(mm[floor((mpx/2)-20):ceiling((mpx/2)+20),c('y','x')])

  theta = lmx$coefficients[2]
  theta=0
  xy = rotate(xyP, theta)

  xy2 = get_int(xy)

  xy2$x = round(xy2$x/res)*res
  xy2 = xy2%>%group_by(x)%>%summarise(ymin=min(y),ymax=max(y))
  xy2 = xy2[c(TRUE,abs(diff(xy2$ymax-xy2$ymin))<0.02),]
  xy2$y = (xy2$ymax+xy2$ymin)/2

  xy2 = xy2[,c('x','y','ymax')]
  names(xy2) = c('x','y','taper')
  xy2$taper = xy2$taper - xy2$y

  xy2$x=xy2$x - min(xy2$x)
  xy2$y=xy2$y - min(xy2$y)

  xy2$y=max(xy2$y) - xy2$y
  xyP$y=max(xyP$y) - xyP$y

  outfn = paste0(outdir,substr(basename(fn),1,nchar(basename(fn))-3), 'sat')
  write.table(xy2[,c('y','x','taper')], outfn, col.names=FALSE, row.names=FALSE)

  L = sum(na.omit(sqrt(diff(xy2$x)^2+diff(xy2$y)^2)))
  La = L / max(xy2$taper)

  p1=ggplot()+
    geom_path(data=xyP, aes(x=x-min(x), y=max(y)-y), lwd=1.2, lty=2)+
    theme_classic()+ coord_equal()
  p2=ggplot()+
    geom_label(aes(x=1.5,y=0.0,label=paste0('L: ', round(L,2),' mm', ' - L/a:', round(La,2))))+
    geom_point(data=xy2, aes(x=x, y=y), size=1.2,col='blue')+
    geom_line(data=xy2, aes(x=x, y=y), lwd=0.2, lty=2)+
    geom_point(data=xy2, aes(x=x, y=y+taper), size=0.1, col='red')+
    geom_line(data=xy2, aes(x=x, y=y+taper))+
    geom_point(data=xy2, aes(x=x, y=y-taper), size=0.1, col='red')+
    geom_line(data=xy2, aes(x=x, y=y-taper))+
    theme_classic()+coord_equal()
  p<-gridExtra::grid.arrange(p1,p2, ncol=1)
  print(p)


  return(data.frame(L=L,La=La))
}
#
# outdir='C:\\Users\\sgastauer\\Desktop\\'
# L = get_mid(fn = "C:\\Users\\sgastauer\\Desktop\\amphipod.txt", outdir=outdir, rot=TRUE, ndens=2000, res=0.005)
#
#
# outdir="C:/Users/sgastauer/Documents/Zonar - DVM size/shapes/"
# L = get_mid(fn = 'C:\\Users\\sgaastuer\\Desktop\\app0.txt', outdir=outdir, rot=FALSE)
# L = get_mid(fn = 'C:\\Users\\sven\\Desktop\\chaeto0.txt', outdir=outdir, rot=TRUE)
#
# shapefn = paste0(outdir, 'appendicularian.sat')
# shapefn = paste0(outdir, 'cop0.sat')
#
# shapefn = paste0(outdir, 'chaeto0.sat')
# shapefn = paste0(outdir, 'krill.sat')
# fname <- paste0(system.file(package="ZooScatR"),"/extdata/configs/config_0.dat")
# para = ZooScatR::read_para(fname)
#
# #set the soundspeed in the surrounding sea water
# misc <- list(cw=1500)
#
# L = get_mid(fn = "C:/Users/sgastauer/Documents/Zonar - DVM size/shapes/calanus.txt", outdir=outdir, rot=TRUE)
# shapefn = paste0(outdir, 'calanus.sat')
# para$shape$prof_name=shapefn
# para$shape$L_a =L$La
# para$shape$L = L$L
# para$shape$axis_sm=1000
# para$shape$taper_sm=0
# para$shape$rho_L=0
# para$shape$order=2
# para$simu$ni=100
# sh = buildpos(para)
# p<-sh$plot
# p+theme(axis.text=element_text(size=30))
# plot_3D(para)
#
#
# L = get_mid(fn = "C:/Users/sgastauer/Documents/Zonar - DVM size/shapes/krill.txt", outdir=outdir, rot=TRUE, ndens=2000, res=0.005)
# shapefn = paste0(outdir, 'krill.sat')
# para$shape$prof_name=shapefn
# para$shape$L_a =L$La
# para$shape$L = L$L
# para$shape$axis_sm=0
# para$shape$taper_sm=0
# para$shape$rho_L=0
# para$shape$order=2
# para$simu$ni=100
# sh = buildpos(para)
# p<-sh$plot
# p+theme(axis.text=element_text(size=30))
#
# plot_3D(para)
#
#
# L = get_mid(fn = "C:/Users/sgastauer/Documents/Zonar - DVM size/shapes/chaetognath.txt", outdir=outdir, rot=FALSE, ndens=2000, res=0.005)
# shapefn = paste0(outdir, 'chaetognath.sat')
# para$shape$prof_name=shapefn
# para$shape$L_a =L$La
# para$shape$L = L$L
# para$shape$axis_sm=0
# para$shape$taper_sm=0
# para$shape$rho_L=0
# para$shape$order=0
# para$simu$ni=100
# sh = buildpos(para)
# p<-sh$plot
# p+theme(axis.text=element_text(size=20))
#
# plot_3D(para)
#
# L = get_mid(fn = "C:/Users/sven/Documents/Zonar - DVM size/shapes/appendicularian.txt", outdir=outdir, rot=TRUE, ndens=2000, res=0.005)
# shapefn = paste0(outdir, 'appendicularian.sat')
# para$shape$prof_name=shapefn
# para$shape$L_a =L$La
# para$shape$L = L$L
# para$shape$axis_sm=100
# para$shape$taper_sm=0
# para$shape$rho_L=0
# para$shape$order=0
# para$simu$ni=100
# sh = buildpos(para)
# p<-sh$plot
# p+theme(axis.text=element_text(size=20))
#
# plot_3D(para)
#
#
# para$simu$var0 = 200
# para$simu$var1 = 1000
# para$simu$n=801
#
#
# para$orient$PDF=2
# para$orient$angm=0
# para$orient$ang0=0
# para$orient$ang1 =5
# para$orient$dang=0.1
#
# para$orient$ave_flag = 1
#
# para$simu$ni=200
# res = bscat(para=para,misc=misc)
# res$rplot
#
# sims = as.data.frame(res$ysim)%>%gather()
# sims$key = as.numeric(substr(sims$key,2,nchar(sims$key)))
# sims$freq = rep(res$var, length(sims$key)/length(res$var))
# sims$theta = rep(res$ang, each=length(res$var))
# pp = ggplot()+
#   geom_line(data=sims, aes(x=freq, y=value, group=key, col=theta), alpha=0.2)+theme_classic()+
#   xlab('Frequency')+ylab('TS (dB re m2)')+
#   scale_color_gradientn(colors=rev(pals::brewer.rdylbu(15)), name=~theta)
# pp<-pp+geom_line(aes(x=res$var, y=res$y), color='red', lwd=2)
# pp+theme(legend.position='top')
#
#
# library(sf)
# plot(pts)
