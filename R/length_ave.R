#' Model output averaged over length
#'
#' @param ka0 original ka  ka=[ <a>-3*std_a  <a>+3*std_a]
#' @param ka1 output ka
#' @param f complex form function - freq.(or ka)  x  - orient. angle
#' @param pdf_type distribution type       1: uniform  2: Gaussian
#' @param paral PDF parameters: paral[1] = no. of bins for L PDF; paral[2] = 1/3 max. deviation for uniform, = std(length) for Gaussian
#' @param app TRUE or FALSE, defines if the function is used within a shiny app or not. If \code{app==TRUE} the progressbar inside the app will be updated.
#' @import pracma
#' @return averaged sigma_bs points
#' @examples
#' ka0=ka
#' ka1=kaL
#' f=f1
#' pdf_type=2
#' paral=len_ave_para
#' length_ave(ka0,ka1,pdf_type,paral)
#' @export

length_ave <- function(ka0,ka1,f,pdf_type,paral, app=FALSE){
  n=length(ka1)    # n = points in freq. (ka)
  m=round(paral[1])
  if(m == 1){
    outy=f
    return(outy)
  }

  r_min = 1 - 3 * paral[2]		# ratio = Lmin/<L>
  r_max = 1 + 3 * paral[2]		# ratio = Lmax/<L>
  L = seq(r_min,r_max,length=m)
  dL = L[2] - L[1]
  if(pdf_type == 1){
    PDF = matrix(1,m,1)/m
  }else{
    Lm=1			#
    Lstd = paral[2]		#      std(L)/<L>
    PDF = dL * exp(-0.5 * (L - Lm)^2 / Lstd^2) / (sqrt(2 * pi) * Lstd)
  }

  sigma_bs0 = f * f		# f sqrt of orientatipon averaged scat. cross-section
  # which is a real function
  sigma_bs = matrix(0, m, n)
  for(j in 1:m){
    ka2=L[j]*ka1

    if(max(ka2) > max(ka0) | min(ka2) < min(ka0)){
      if(app==FALSE){
        print('ka is beyond ka0')
        print(paste(c(min(ka0), min(ka2),  max(ka2),  max(ka0))))
      }else{
        shiny::showNotification(paste("ERROR: ka is beyond ka0",(c(min(ka0), min(ka2),  max(ka2),  max(ka0)))), type="error")
      }
  }else{
    sigma_bs[j, 1:n] = L[j] * L[j] * PDF[j] * pracma::interp1(as.numeric(ka0),
                                                      as.numeric(sigma_bs0),
                                                      as.numeric(ka2))
  }
}

outy = sqrt(colSums(sigma_bs))
return(outy)
}
