#' inhom_gh
#' @description construct inhomogeneous g and h based on given parameter
#' @param para set of parameters
#' @return Returns a list containing the constructed g, h, Cb, FlucVal (fluctuation value), ghplot (a plot of the newly generated g and h), flucplot (a plot of the fluctuation function), gh.sum (a summary of the new gh values)
#' @import ggplot2
#' @import pracma
#' @examples
#' new_gh <- inhom_gh(para)
#' new_gh$flucplot
#' new_gh$ghplot
#' @export

inhom_gh <- function(para){

if(exists("inhom_g")==FALSE){inhom_g <- matrix()}
if(exists("inhom_h")==FALSE){inhom_h <- matrix()}

n_int <- para$simu$ni
g0 <- para$phy$g0
h0<-para$phy$h0
SegNo<-para$phy$seg_no
lcorr<-round(para$phy$corrL*n_int/100)
VarStd_g<-para$phy$g_std
VarStd_h<-para$phy$h_std
prop_fac<-VarStd_h/VarStd_g

if(is.null(para$phy$multiseg)){para$phy$multiseg <-0}
if(para$phy$body_ih==TRUE){para$phy$multiseg=1}

if(para$phy$multiseg == 0){
  g<-g0*matrix(1,n_int,1)
  h<-h0*matrix(1,n_int,1)
  FlucVal<-matrix(0,n_int,1)
  Cb=(1-g*h*h)/(g*h*h)-(g-1)/g
}

#figure(2)

if (lcorr < 0.001 | SegNo <= 8){ #construct un-correlated randomized g ang h profile
  #disp(' < 7 segment ')
  OK = 0

  while (OK != 1){
    if (VarStd_g  > 1.e-8){
        NoPerSeg=pracma::ceil(n_int/SegNo)     #Integration points in each segment
        FlucVal=VarStd_g*rnorm(SegNo)
        Valstd = sd(FlucVal)
        Valmean = mean(FlucVal)
        #adjust random variable to meet <>=0 sdt()= VarStd
        FlucVal=FlucVal-Valmean
        FlucVal=VarStd_g*FlucVal/Valstd
        Valstd=pracma::std(FlucVal)
        Valmean=mean(FlucVal)

        #disp(sprintf('g: mean = %g',Valmean))
        #disp(sprintf('g: std = %g',Valstd))

      for (j in 1:n_int){
        k=min(floor(j/NoPerSeg)+1,length(FlucVal))
        inhom_g[j]=FlucVal[k]
      }
    }else{
        inhom_g=zeros(n_int,1)
    }

     g=g0+inhom_g

     if (prop_fac == 0){			#g & h no correlation

       if (VarStd_h  > 1.e-8){
         NoPerSeg=pracma::ceil(n_int/SegNo)     #Integration points in each segment
         FlucVal=VarStd_h*rnorm(SegNo)
         Valstd=sd(FlucVal)
         Valmean=mean(FlucVal)
         #adjust random variable to meet <>=0 sdt()= VarStd
         FlucVal=FlucVal-Valmean
         FlucVal=VarStd_h*FlucVal/Valstd
         Valstd=sd(FlucVal)
         Valmean=mean(FlucVal)

         #disp(sprintf('h: mean = %g',Valmean))
         #disp(sprintf('h: std = %g',Valstd))
         for (j in 1:n_int){
          k=min(floor(j/NoPerSeg)+1,length(FlucVal))
          inhom_h[j]=FlucVal[k]
          }
        }else{
         inhom_h=zeros(n_int,1)
        }

       h=h0+inhom_h
    }else{               #g and h are correlated
       h=h0+prop_fac*inhom_g
    }

     #g=g
     #h=h
    Cb=(1-g*h*h)/(g*h*h)-(g-1)/g
    ft=FlucVal

    #plot(1:n_int,g,1:n_int,h)

    #s=input('OK (y/n)?  ','s')
    #s='y'
    #s = 'y'
    OK=1

     #     fname=input('Specify the FileName to save this profile =  ','s')
     #     cmd=['save ' fname ' g h ft VarStd_g VarStd_h n_int SegNo']
     #     eval(cmd)
  #}
#}
}
}else{		#construct correlated randomized g ang h profile

  #Gaussian correlation function
z=seq(0,n_int,length = n_int)-n_int/2
Rg = exp(-0.5%*%(z/lcorr)^2)  #sqrt(correlation function)
P1=fft(Rg)
Pphs=Arg(P1)
P=abs(P1)
F=as.matrix(sqrt(P))
if (pracma::rem(n_int,2) == 0){ 		#n_int is an even number
  n=n_int/2
  phs=2*pi*pracma::rand(1,n-1)
  F[2:n]=F[2:n]*exp(1i*phs) #negative frequency component
  phs_flip=-pracma::fliplr(phs)     #fliped phase
  F[(n+2):n_int]=F[(n+2):n_int]*exp(1i*phs_flip) #complex conjugate (pos. freq)
  }else{                           #n_int is an odd number
    n=(n_int-1)/2
    phs=2*pi*pracma::rand(1,n)
    F[2:(n+1)]=F[2:(n+1)]*exp(1i*phs)			       #
    phs_flip=-pracma::fliplr(phs)
    F[(n+2):n_int]=F[(n+2):n_int]*exp(1i*phs_flip)   #complex conjugate
    }
ftc=pracma::ifft(as.vector(F))
ft=Re(ftc)
ft=ft-mean(ft)
fac=VarStd_h/pracma::std(ft)
ft=fac*ft

#disp(sprintf('std(ft) = %g, <ft> = %g',std(ft),mean(ft)))
#check
F1=fft(ft)
Pc=F1*Conj(F1)*exp(1i*Pphs)
Rx=acf(ft)
#subplot(311)
ft_df <- as.data.frame(cbind(x=seq(1,length(ft)), y=ft))
flucplot <- ggplot2::ggplot(ft_df,
                            ggplot2::aes(x,y))+
  ggplot2::geom_step(lwd=1.2)+
  ggplot2::ylab('Fluctuation Func.')+
  ggplot2::xlab("")+
  ggplot2::coord_cartesian(expand = c(0.2,0.2))+
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text = ggplot2::element_text(size=18),
        axis.title = ggplot2::element_text(size=18))


#grid
  Rf=Re(pracma::ifft(as.vector(Pc)))				#real function
  indx=1:n_int
  #subplot(312)
  hh_gg <- as.data.frame(cbind(x=indx,
                               y=Rx$acf[(n_int-n):(n_int+n-1)]))
  hh=ggplot2::ggplot(data=hh_gg,
                     ggplot2::aes(x=x,y=y))+
    ggplot2::geom_step()+
    ggplot2::xlab('Index')+
    ggplot2::ylab('Constructed g and h')+
    ggplot2::coord_cartesian(expand = c(0.2,0.2))+
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.text = ggplot2::element_text(size=18),
          axis.title = ggplot2::element_text(size=18)) #,indx,Rf,'r')
  #legend(hh,'Reconstructed Rx','Original Rx')
  #ylabel('AutoCorrelation Func.')
  #grid

#subplot(313)
g=g0*(1+ft)
h=h0*(1+prop_fac*ft)
hh_gg <- as.data.frame(cbind(x=indx,
                             g=g,
                             h=h))
hh=ggplot2::ggplot(data=hh_gg)+
  ggplot2::geom_step(ggplot2::aes(x=x,y=g),col="red")+
  ggplot2::geom_step(ggplot2::aes(x=x,y=h),col="blue")+
  ggplot2::xlab('Index')+
  ggplot2::ylab('Constructed g and h')+
  ggplot2::coord_cartesian(expand = c(0.2,0.2))+
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text = ggplot2::element_text(size=18),
        axis.title = ggplot2::element_text(size=18))

#grid
scl=axis
xt=0.2*scl(2)
yt=scl(4)-scl(3)
#text(xt,0.9*yt+scl(3),sprintf('<g> = %6.5g',g0))
#text(0.4*scl(2),0.9*yt+scl(3),sprintf('<h> = %6.5g',h0))
ft=ft
FlucVal=ft
g=g
h=h

Cb=(1-g*h*h)/(g*h*h)-(g-1)/g
}

inh.df <- as.data.frame(cbind("g"=g,"h"=h))
names(inh.df)<- c("g", "h")
inh.df$index=1:length(inh.df[,1])
inh.df <- reshape2::melt(inh.df, id.var="index")

ghplot <- ggplot2::ggplot(data=inh.df, ggplot2::aes(x=index, y=value, group=variable, col=variable))+
  ggplot2::scale_color_discrete(name="")+
  ggplot2::geom_path(lwd=1.5)+
  ggplot2::xlab("Index")+
  ggplot2::ylab("Constructed g and h")+
  ggplot2::coord_cartesian(expand = c(0.2,0.2))+
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text = ggplot2::element_text(size=18),
        axis.title = ggplot2::element_text(size=18),
        legend.position = c(0.05,0.97))

gh.sum <- t(as.data.frame(cbind("No. of Seg"=para$phy$seg_no,
                              "g"=mean(g),
                              "sd(g)"=sd(g),
                              "h"=mean(h),
                              "sd(h)"=sd(h),
                              "Correlation Length"=para$phy$corrL)))

if(exists('flucplot')==FALSE){flucplot=NULL}

  inh <- list(g=g,h=h,Cb=Cb,FlucVal=FlucVal, ghplot = ghplot, flucplot=flucplot, gh.sum=gh.sum)
return(inh)
}
