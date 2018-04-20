#' @title Build shape vector of the hypothetical taget
#' @description Builds all the coordinate vectors related to the shape positions of the target
#' body axis is along the z-axis
#' construct shape coordinates
#' a list containing the shape coordinates: r_pos (position vector), the_tilt , dr, gamma_t, taper, x, z, and plot if disp_prof=1
#' @import  pracma
#' @import ggplot2
#' @param para a list containining the shape parameters
#' @param para$shape$ni Number of integration points
#' @param para$shape$order Tapering order
#' @param para$shape$rho_L rho = radius of curvature
#' @param para$shape$prof_name Custom shape, -1 for regular shape, filename for custom shape
#' @param disp_prof if 1 a ggplot of the shape is added to the output list, otherwise, no plot is produces
#' @examples para$shape$L <-30
#' para$shape$L_a <- 16
#' para$shape$rho_L <- 3
#' para$shape$order <- 7

buildpos <- function(para, disp_prof=1){
  eps<-2^(-52)

  #print(paste("Buildpos:",para$shape$prof_name))
  #r_pos,th_tilt,dr,gamma_t,taper,x,z){
  n_int  =  para$simu$ni
  order = para$shape$order
  rho_L = para$shape$rho_L
  L_a = para$shape$L_a


  if(is.null(para$shape$prof_name)){para$shape$prof_name=-1}
  if (para$shape$prof_name==-1){		# regular shape
    # uniformly bent cylinder and regularly tapered
    gamma = 0.5/rho_L
    ratio = 2*rho_L
    z = seq(-1,1,length = n_int)
    taper = sqrt(1-z^order)
    # normalized by rho -  radius of curvature
    z = sin(gamma)*z
    x = (1-sqrt(1-z*z))
    # normalized by L/2
    x = ratio*x
    z = ratio*z
  }else{				# arbitrary position profile and arbitrary tapering
    pos = read.table(para$shape$prof_name, header = FALSE)
    xp = pos[,1]
    zp = pos[,2]
    taper1 = pos[,3]
    z = seq(min(zp) + eps, max(zp)-eps, length=n_int)
    x = pracma::interp1(zp, xp, z, method="linear")
    taper = pracma::interp1(zp, taper1, z, method="linear")
    x = x - mean(x)
    z = z - mean(z)
    dr = sqrt(diff(x)^2 + diff(z)^2)
    L = sum(dr)
    x = 2*x/L
    z = 2*z/L							# normalized position vector
    para$shape$x = x
    para$shape$z = z
    para$shape$xp = xp
    para$shape$zp = zp
    para$shape$taper = taper1
  }

  taper = matrix(taper,1,nrow=n_int)

  if (para$shape$prof_name!=-1){
    x = smoother(x,para$shape$axis_sm)
  }

  if (disp_prof  ==  1){
    taper0 = taper
  }


  if (para$shape$prof_name != -1){
    taper = smoother(as.numeric(taper),para$shape$taper_sm)
  }

  th_tilt = matrix(0, n_int, 1)
  r_pos = sqrt(x * x + z * z)
  gamma_t = atan2(z, x)
  dx = diff(x) + eps
  dz = diff(z)
  alpha_t = c(atan(dz / dx), atan(dz[n_int-1]/dx[n_int-1]))
  indx1 = which(alpha_t < 0)
  if(length(indx1) > 0){
    th_tilt[indx1] = alpha_t[indx1] + pi / 2
  }

  indx2 = which(alpha_t >=  0)
  if(length(indx2) > 0){
    th_tilt[indx2] = alpha_t[indx2]-pi/2
  }

  dr1 = sqrt(dx * dx + dz * dz)
  dr = c(dr1[1], dr1)

  if(disp_prof  ==  1){
    x1 = x+taper/L_a
    x2 = x-taper/L_a
    x10 = x+taper0/L_a
    x20 = x-taper0/L_a
    ppp <- ggplot2::ggplot()+
      ggplot2::geom_path(
        ggplot2::aes(y=x,x=z))+
      ggplot2::geom_path(
        ggplot2::aes(y=as.numeric(x1),x=z))+
      ggplot2::geom_path(
        ggplot2::aes(y=as.numeric(x2),x=z))+
      ggplot2::geom_path(
        ggplot2::aes(y=as.numeric(x10),x=z), lty=2, lwd=1.2)+
      ggplot2::geom_path(
        ggplot2::aes(y=as.numeric(x20),x=z),lty=2, lwd=1.2)+
      ggplot2::coord_fixed(ratio=2,expand = c(0.2,0.2))+
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border =
                       ggplot2::element_blank(),
            panel.grid.major =
              ggplot2::element_blank(),
            panel.grid.minor =
              ggplot2::element_blank(),
            axis.line =
              ggplot2::element_line(colour = "black"),
            axis.text =
              ggplot2::element_text(size=18),
            axis.title =
              ggplot2::element_text(size=18))
    #  axis('equal')grid
    # disp('press any key to cont. ..')pause
    # plot(z,taper0,z,taper,'r')
  }
  out =list()
  out$r_pos=r_pos
  out$th_tilt = th_tilt
  out$dr = dr
  out$gamma_t = gamma_t
  out$taper = taper
  out$x = x
  out$z = z
  out$plot <- ppp
  return(out)
}


