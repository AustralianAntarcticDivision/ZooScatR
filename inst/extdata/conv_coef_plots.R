#' Create convolution matrix
#' @import ggplot2
#' @import cowplot
#' @import viridis
#' @import reshape2
#' @import pbmcapply
#' @param in_fn RDS file with the TS information
#' @param out_f folder where the plots should be saved
#' @return 2 convolution coefficient plots
#' @example
#'#TS look out
conv_coef <- function(in_fn, out_f){
  TS.df <- readRDS(paste0(in_fn,".RDS"))
  f.cols <- 1:(length(TS.df)-3)
    TS.df[,f.cols] <- 10^(TS.df[,f.cols]/10)
    cor_coef <- function(x){
      v1 = as.vector(t(TS.df[x[1],f.cols]))
      v2 = as.vector(t(TS.df[x[2],f.cols]))
      convolve(v1,v2, type='filter')/
        sqrt(convolve(v1,v1, type='filter')*
               convolve(v2,v2,type='filter'))
    }

    vv <- which(TS.df$L %in% c(30,40,50,60) & TS.df$theta %in% c(-90,-60,-30,0))

    db <- cbind(x=rep(vv,length(TS.df[,1])),
                y=rep(1:length(TS.df[,1]),each=length(vv)))
    #create list to run lapply in parallel
    db.list <- split(db, seq(nrow(db)))
    #run lapply in aprallel with progressbar
    res2 <- as.vector(t(as.data.frame((pbmclapply(db.list,cor_coef,mc.cores=6)))))

    res.df <- as.data.frame(cbind(x=db[,1], y=db[,2],coef=res2))
    res.df$label <- paste("L =",TS.df$L[res.df$x],"Theta = ",TS.df$theta[res.df$x])
    res.df$L <-TS.df$L[res.df$y]
    res.df$theta <-TS.df$theta[res.df$y]
    res.df$L_o <- TS.df$L[res.df$x]
    res.df$theta_o <- TS.df$theta[res.df$x]

    #sub.df = res.df[res.df$L_o %in% c(30,40,50,60),]
    sub.df = res.df
    coef.p <- ggplot(data=sub.df,aes(x=theta,y=L,fill=coef,z=coef))+
      geom_raster(interpolate = FALSE,alpha=0.7)+
      stat_contour(col="black",binwidth=0.1) +
      facet_grid(L_o~theta_o)+
      scale_fill_viridis(name="Conv. Coefficient\n")+
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      xlab("Theta")+ylab("Length")+
      theme(legend.position = "top",
            axis.text = element_text(size=26),
            axis.text.x = element_text(size=26,angle=75,hjust=1),
            axis.title = element_text(size=26),
            strip.background = element_rect(
              color="black", fill="white", size=1.5, linetype=0),
            strip.text = element_text(size = 26))
    coef.p
    save_plot(paste0(out_f,"/conv_coef_red.png"),coef.p,base_height=12,base_width=18)

    vv2 <- which(TS.df$L %in% c(30,40,50,60) & TS.df$theta %in% c(seq(-90,0,by=10)))

    TS <- melt(TS.df[vv2,-which(names(TS.df)=="id")], id.vars=c("L","theta"))
    TS$Frequency <- as.numeric(as.character(TS$variable))
    TS$label <- paste("L =",round(TS$L/10,0)*10,"Theta = ",round(TS$theta/10,0)*10)
    TS$value <- 10*log10(TS$value)

    TS.plot <-
      ggplot(data=TS, aes(x=Frequency,y=value, fill='value'))+
      geom_path(size=1.2)+
      facet_grid(L~theta)+
      xlab("Frequency [kHz]")+ylab('TS [dB]')+
      theme(axis.text = element_text(size=26),
            axis.text.x = element_text(size=26,angle=75,hjust=1),
          axis.title = element_text(size=26),
          strip.background = element_rect(color="black", fill="white", size=2, linetype=0),
          strip.text = element_text(size = 26))
    TS.plot
    save_plot(paste0(out_f,"/TS_spectra_red.png"),TS.plot,base_height=12,base_width=18)
}

