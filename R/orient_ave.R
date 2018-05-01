#' averaging over orientation
#' @param ang incident angle
#' @param f complex form function as a function of frequency, ka or orient. angle
#' @param pdf_type distribution type       1: uniform  2: Gaussian
#' @param paral PDF parameters: paral[1] = angle; paral[2] = range for uniform, = std for Gaussian
#' @import pracma
#' @export
#' @author Sven Gastauer
#' @return averaged y points
#' @examples
#' averaging over orientation
#' paral = orient_ave_para
#' f=f
#' pdf_type = para$orient$PDF
#' paral = orient_ave_para
#' paral=orient_ave_para
#' orinet_ave(ang, f, pdf_type, paral)

orient_ave <- function(ang,f,pdf_type,paral){
  # ang =       incident angle
  # f   =       complex form function   # freq.  x  # orient. angle
  # pdf_type =  distribution type       1: uniform  2: Gaussian
  # paral =      PDF parameters:
  #         para(1) = <angle>
  #         para(2) = range          for uniform
  #		  = std(angle)     for Gaussian
  #

  n = dim(f)[1] # n = points in freq.,
  m = dim(f)[2] # m = points in orientation

  #disp([n m])
  if(m == 1){
    outy=f
    return(outy)
  }
  if(pdf_type == 1){
    PDF=pracma::ones(m,1)/m
  }else{
    dang=ang[2]-ang[1]
    angm=paral[1]
    angstd=paral[2]
    PDF=dang*exp(-0.5*(ang-angm)^2/angstd^2)/(sqrt(2*pi)*angstd)
  }

    outy=sqrt((f*Conj(f))%*%PDF)

    return(outy)
}

