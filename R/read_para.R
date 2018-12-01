#' Read the configuration file
#' @description Read a DWBA configuration file. Keep standard .dat format, as used in Matlab, to keep maximum compatibility with previous model definitions.
#'
#' .dat files are standard text files.
#'
#' Some example config files can be found in the extdata/configs folder: (\code{system.file(package="ZooScatR")}), contained within the package directory.
#'
#' @param fn Filename
#' @return list with all parameters for DWBA
#' @examples
#' fn <- paste0(system.file(package="ZooScatR"),"/extdata/configs/config_0.dat")
#' para <- read_para(fn)

read_para <- function(fn){
  #read all lines
  lns = readLines(fn)
  #extract heder indices
  idx = grepl("%%", lns)
  #read filename
  filename = lns[!idx][1]

  #data
  data <- na.omit(lns[!idx][2:length(idx)])
  data <- gsub(",\t", ",", data)
  data <- gsub("%\t\t%", "NA %", data)
  data <- gsub("\t\t", " ", data)
  data <- gsub("\t", " ", data)
  elems = unlist( strsplit( data , "%" ) )
  #create dataframe
  parameters <- as.data.frame(matrix( elems , ncol = 2 , byrow = TRUE ))

  pn <- as.character(parameters[8,1])
  pn <- gsub(" ","", pn)

  #values as numeric
  suppressWarnings(parameters[,1] <- as.numeric(as.character(parameters[,1])))


  #create list
  para <- list()

  #filename
  para$fname <- filename

  #shape
  para$shape <- list()
  para$shape$L <- parameters$V1[1] #length in mm
  para$shape$rho_L <- parameters$V1[2] #rho/L
  para$shape$L_a <- parameters$V1[3] #L/a
  para$shape$order <- parameters$V1[4] #tapering order
  para$shape$ave_flag <- parameters$V1[5] #length average flag: 0-no average,1-average
  para$shape$Lstd <- parameters$V1[6] #relative length standard deviation (meaningless for no average)
  para$shape$dL <- parameters$V1[7] #relative length increment(meaningless for no average)
  para$shape$prof_name <- parameters$V1[8] #shape profile name ( "-1" if profile is not specified)
  if(is.na(para$shape$prof_name)==TRUE){para$shape$prof_name=pn}
  para$shape$axis_sm <- parameters$V1[9] #number of points for axis smoothing (meaningless if profile is not specified)
  para$shape$taper_sm <- parameters$V1[10] #number of points for tapering function smoothing (meaningless if profile is not specified)

  #orientation
  para$orient <- list()
  para$orient$angm <- parameters$V1[11] #mean incident angle (deg)
  para$orient$ang0 <- parameters$V1[12] #angle variation range - starting value (deg)
  para$orient$ang1 <- parameters$V1[13] #angle variation range - ending value (deg)
  para$orient$ave_flag <- parameters$V1[14] #average option flag: 0-no average,1-average
  para$orient$PDF <- parameters$V1[15] #PDF flag: 1-Uniform 2-Gaussian
  para$orient$PDF_para <- parameters$V1[16] #PDF parameter: half range for Uniform PDF,
                                       #standard deviation for Gaussian (deg),
                                       #(meaningless for no average)
  para$orient$dang  <- parameters$V1[19] #angle increment (meaningless for no average)

  #physical
  para$phy$g0  <- parameters$V1[20] #mean density contrast (g0)
  para$phy$h0  <- parameters$V1[21] #mean sound speed contrast (h0)
  para$phy$seg_no  <- parameters$V1[22] #number of body segments (1-homogeneous body)
  para$phy$g_std  <- parameters$V1[23] #standard deviation of g0 (meaningless for homogeneous body)
  para$phy$h_std  <- parameters$V1[24] #standard deviation of g0 (meaningless for homogeneous body)
  para$phy$corrL  <- parameters$V1[25] #correlation length as a percentage of body length L

  #simulation
  para$simu$ni <- parameters$V1[26] #integration points along body axis
  para$simu$n <- parameters$V1[27] #number of output points
  para$simu$out_indx <- parameters$V1[28] #output variable index: 1-scatting amplitude, 2-cross section, 3-TS, 4-RTS
  para$simu$var_indx <- parameters$V1[29] #variable: 1-frequency (kHz), 2-ang(deg),  3-ka
  para$simu$var0 <- parameters$V1[30] #start value for the variable
  para$simu$var1 <- parameters$V1[31] #end value for the variable
  para$simu$freq <- parameters$V1[32] #frequency (kHz), enabled for avaiable being angle only

  return(para)
}
