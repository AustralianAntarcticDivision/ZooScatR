#' @title Simulation average backscattering over orienation and length by euphausiid and copopod
#' @param para Model Parameters
#' @param app = FALSE [boolean] function call from shiny interface or command line
#' @author Sven Gastauer
#' @return list with all parameters for DWBA
#' @import ggplot2
#' @export
#' @example
#'
bscat <- function(para, misc, app=FALSE){
  if(exists("status")==FALSE){status=list()}
  #print(misc)
  #print(para)
  status$stop = 0
  # limits of angle variation
  if (para$simu$var_indx == 2){
    if(para$orient$ave_flag == 1){
      if(para$orient$PDF == 1){	# uniform PDF
        ang_max=para$simu$var1+para$orient$PDF_para
        ang_min=para$simu$var0-para$orient$PDF_para
      }else{								# Gaussian PDF
        ang_max=para$simu$var1+3*para$orient$PDF_para
        ang_min=para$simu$var0-3*para$orient$PDF_para
      }
      if(ang_min < para$orient$ang0){
        para$orient$ang0 = ang_min
        #h = findobj(gcf,'Tag','EditTextTheta0')
        #set(h,'String',num2str(para$orient$ang0)) ################### To be fixed
      }
      if(ang_max > para$orient$ang1){
        para$orient$ang1=ang_max
        #h=findobj(gcf,'Tag','EditTextTheta1')
        #set(h,'String',num2str(para$orient$ang1))################### To be fixed
      }
      ang = seq(ang_min, ang_max, by = para$orient$dang)
    }else{
      ang_min = para$simu$var0
      ang_max = para$simu$var1
      ang=seq(ang_min,ang_max,length=para$simu$n)
    }
  }else{
      if(para$orient$ave_flag == 1){
      if(para$orient$PDF == 1){		# uniform PDF
        ang_min = para$orient$angm - para$orient$PDF_para
        ang_max = para$orient$angm + para$orient$PDF_para
    }else{								# Gaussian PDF
      ang_min = para$orient$angm - 3.1 * para$orient$PDF_para
      ang_max = para$orient$angm + 3.1 * para$orient$PDF_para
    }
      ang = seq(ang_min,ang_max, by = para$orient$dang)
    }else{
      ang_min=para$orient$angm
      ang_max=para$orient$angm
      ang=ang_min
    }
}
# limits of ka variation
a = para$shape$L / para$shape$L_a # mm	# a in mm and freq in kHz
if(para$simu$var_indx == 1){
  ka0 = 2 * pi * para$simu$var0 * a / misc$cw		# a (mm) f(kHz)
  ka1 = 2 * pi * para$simu$var1 * a / misc$cw
}
if(para$simu$var_indx == 2){
  ka0 = 2 * pi * para$simu$freq * a / misc$cw
  ka1 = ka0
}
if(para$simu$var_indx == 3){
  ka0 = para$simu$var0
  ka1 = para$simu$var1
}

if ( para$shape$ave_flag == 1){
  ka_min = ka0 * (1 - 3.1 * para$shape$Lstd)
  ka_max=ka1*(1+3.1*para$shape$Lstd)
  nl=6*para$shape$Lstd/para$shape$dL
}else{
  ka_min = ka0
  ka_max = ka1
  nl = 1
}

len_ave_para = c(nl, para$shape$Lstd)

Npts = para$simu$n

if(para$simu$var_indx == 2){
  if(para$shape$ave_flag == 1){
    nl = 6 * para$shape$Lstd / para$shape$dL
    kaL = seq(ka0, ka1, length=1)
    ka = seq(ka_min, ka_max,length=nl)
  }else{
    ka = ka0
    kaL = ka
  }
 }else{
   if(Npts == 1){
     ka = ka_min
     kaL = ka0
    }else{
      ka= seq(ka_min, ka_max, length=Npts)
      kaL = seq(ka0, ka1, length=Npts)
  }
}

misc$ang = ang
misc$ka = ka

# conmpute scattering amplitude/L
if(is.null(para$shape$profile)){para$shape$profile = -1}
if(is.null(para$phy$body_ih)){para$phy$body_ih = FALSE}

#define if in script mode or app mode
if(exists('app')==FALSE){app="script"}
dwba_out=DWBAscat2(para, misc, app)
ka = dwba_out$ka
ang = dwba_out$ang
f = dwba_out$f

if(status$stop == 1){
  return()
}

if(para$simu$var_indx == 2){
  angm = seq(para$simu$var0, para$simu$var1, length=Npts)	# mean incident angle
  len_ave_para = c(nl, para$shape$Lstd)
  for(i in 1:Npts){
      orient_ave_para = c(angm[i], para$orient$PDF_para)
      if(para$orient$ave_flag == 1){
        f1 = orient_ave(ang,f,para$orient$PDF,orient_ave_para)
      }else{
        f1 = f[,i]
      }
  if(exists("f2")==FALSE){f2<-NA}
  f2[i] = length_ave(ka,kaL,f1,2,len_ave_para, app=TRUE)
  }
}else{
  orient_ave_para = c(para$orient$angm, para$orient$PDF_para)
  f1 = orient_ave(ang,f,para$orient$PDF,orient_ave_para)
  len_ave_para = c(nl, para$shape$Lstd)
  f2 = length_ave(ka,kaL,f1,2,len_ave_para, app=TRUE)
}

# convert output to the specified quantity
y = abs(f2)
ylab=expression(paste('Normalised Backscattering Amplitude ', f[bs*L^{-1}]))
if(para$simu$out_indx == 2){
  y = y*y
  ylab= expression(paste("Scattering Cross Section ", sigma["bs"]))
}

if(para$simu$out_indx == 3){
  y=20*log10(y*para$shape$L)-60	# L: mm -> m
  ylab=expression(paste('Target Strength (dB re ', m^{2},')'))
}

if(para$simu$out_indx == 4){
  y=20*log10(y)
  ylab=expression(paste('Reduced Target Strength (dB re ', m^{2},')'))
}

xlabs <- c('Frequency (kHz)','Orientation angle','ka')
xlab <- xlabs[para$simu$var_indx]

var = seq(para$simu$var0, para$simu$var1, length=para$simu$n)
p = ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=var,y=y),lwd=1.5)+
  ggplot2::xlab(xlab)+
  ggplot2::ylab(ylab)+
  ggplot2::scale_y_continuous(expand=c(0.2,0.2))+
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border =
                   ggplot2::element_blank(),
                     panel.grid.major =
                   ggplot2::element_blank(),
                     panel.grid.minor =
                   ggplot2::element_blank(),
        axis.line =
          ggplot2::element_line(colour = "black"))

#xlabel(xlab);
#ylabel(ylab);
#dat.var=var;
#dat.fun=y;
#p=0;

return(list(var = var, y=y,rplot=p, xlab=xlab, ylab=ylab, shplot = dwba_out$shplot))
}
