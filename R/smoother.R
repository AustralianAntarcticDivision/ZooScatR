#' @title Smoothing function
#' @description Smoother function used to build the shape of the targets when axis or taper smoothing is requested.
#' @author Sven Gastauer
#' @param yi Vector of values to be smoothed
#' @param n Number of points contained in the output y
#' @return y vector of smoothed y points
#' @export
#' @examples
#' yi <- sample(1:1000,100)
#' n <- 4
#' y <- smoother(yi,n)
#' @export

smoother <- function(yi,n){
  if(n <= 1){
    y=yi
    return(y)
  }

  y=yi
  nl=length(yi)

  if(n > nl){
      n=nl
  }

  m=floor(n/2)

  for(i in 1:m){
    #y[i] = mean(yi[max(1,(i-m)):i+m], na.rm=T) }
    y[i] = mean(yi[max(1, (i-m)) : (i+m)])}

  for(i in seq((m+1),(nl-m))){
    #y[i] = (mean( yi[(i-m) : (i+m)], na.rm=T) )
    y[i] = mean( yi[ (i-m) : (i+m) ])}

  for(i in (nl-m+1):nl){
    #y[i] = mean(yi[(i-m) : (min(i+m,nl))], na.rm=T)
    y[i] = mean(yi[(i-m) : min( (i+m), nl )], na.rm=T)
}

  return(y)
}
