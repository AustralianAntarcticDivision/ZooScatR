#' @title  create_profile
#' @description Create a target material properties profile file \cr
#'
#' Generate a profile based on a list of g and h values, number of segments and a length correlation
#' @param g Density contrast g
#' @param h Soundspeed contrast h
#' @param seg_no Number of segments
#' @param corrL Length Correlation factor
#' @param filename path and name .RData file to be saved
#' @return Returns a list with all g, h, segment number and length correlation information, which can be used as an input to the material properties of the DWBA model
#' @examples
#' create_profile(h = c(rep(1.0281,28),
#'                      rep(1.0296,29),
#'                      rep(1.0288,29),
#'                      rep(1.0292,29),
#'                      rep(1.0256,29),
#'                      rep(1.0264,29),
#'                      rep(1.0275,27)),
#'                g = c(rep(1.0359,,28),
#'                      rep(1.0375,29),
#'                      rep(1.0367,29),
#'                      rep(1.0371,29),
#'                      rep(1.0332,29),
#'                      rep(1.0341,29),
#'                      rep(1.0353,27)),
#'                seg_no =7,
#'                corrL = 20,
#'                filename="profile1")

create_profile <- function(g,h,seg_no, corrL, filename){
  #check if filename ends with .RData and add it not
  if(substr(filename,nchar(filename)-1, nchar(filename))!= ".RData"){filename = paste0(filename, ".RData")}
  #create and save the profile list
  profile <- list(h=h,
                  g=g,
                  seg_no=seg_no,
                  corrL=corrL)
  save(profile,
       file=filename)
  #Check if the profile was created succesfully
  if(!file.exists(filename)){
    message("ERROR: Profile could not be generated")
    }else{
      message(paste0("Profile saved as ", filename))
    }
}
