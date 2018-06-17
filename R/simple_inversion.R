#' @title Simple TS inversion through minimisation of sum of squares
#' @description Takes data TS and a data frame containing simulated TS at known Length and Orientation to find the closest matching parameters
#' @author Sven Gastauer
#' @export
#' @param TS.test Dataframe with TS values, where column names are the frequencies
#' @param TS.mod Dataframe with modelled TS values, where each row is a sample and column names are the frequencies, the last columns contain the parameters
#' @param n.nf number of factors contained within the data frame
#' @examples
#' #load data
#' setwd('C:\\Users\\sveng\\Nextcloud\\ts keras\\')
#' TS.mod <- readRDS("data\\TS_sim.RDS")
#' #select random row
#' rr<-sample(1:nrow(TS.mod),1)
#' TS.inv <- TS.inverse.simple(TS.test=TS.mod[rr,1:(length(TS.mod)-3)],
#'                            TS.mod=TS.mod,n.nf=3)
#'  ##########################################################
#' ## CREATE PLOTS
#' ##########################################################
#' ggplot(data=TS.inv,aes(x=L,y=theta,fill=log10(ss)))+
#'   geom_raster(interpolate=TRUE)+
#'   scale_fill_viridis(name="Log10(Sum of Squares)")+
#'   scale_x_continuous(expand=c(0,0))+
#'   xlab("Length [mm]")+
#'   ylab(expression(paste("Incident Angle", theta, " [",degree,"]")))+
#'   scale_y_continuous(expand=c(0,0))+
#'   theme_minimal()+
#'   theme(axis.text = element_text(size=16),
#'         axis.title = element_text(size=18),
#'         legend.position = "top")

TS.inverse.simple <- function(TS.test,TS.mod,n.nf=3){

  #########################################################
  # PREPARE THE MODELLED DATA
  #########################################################

  #split model into data and target
  #TS.mod <- TS.mod[sample(1:nrow(TS.mod),1000),]
  TS.dat <- TS.mod[,1:(length(TS.mod)-n.nf)]
  TS.target <- TS.mod[,(length(TS.mod)-(n.nf-1)):length(TS.mod)]

  ##########################################################
  ## CHECK IF THE SAME FREQUENCIES ARE AVAILABBLE
  ##########################################################

  #Match frequencies
  test.freq <- as.numeric(as.character(names(TS.test)))
  mod.freq <- as.numeric(as.character(names(TS.dat)))

  #Check if length of frequencies and frequencies do match
  if(length(test.freq)!=length(mod.freq) ||
     length((which(test.freq != mod.freq)))>0 ||
     length(which(is.na(TS.test)))>0){

       message(paste0(Sys.time(),": Frequencies of input and modelled data do not
match or NAs detected....\n
               Interpolating data..."))
       #approximate data to fit the modelled data frequencies
       TS.test =as.data.frame(approx(test.freq,
                                 TS.test,
                                 xout = mod.freq,
                                 rule=2))

       }else{
         message(paste0(Sys.time(),": Frequencies of input and modelled data are
                 matching..."))
      }
  ##########################################################
  ## CONVERT TS DATA FROM LOG TO LINEAR DOMAIN
  ##########################################################

  #convert TS data to linear
  TS.lin <- 10^(TS.dat/10)
  dat.lin <- 10^(TS.test/10)

  ##########################################################
  ## COMPUTE SUM OF SQUARES
  ##########################################################
  message(paste0(Sys.time(),": Computing sum of squares..."))
  start_time <- Sys.time()
  ss = apply(TS.lin,1,function(x)sum((dat.lin-t(x))^2))
  end_time <- Sys.time()
  message(paste0(Sys.time(),": COmpleted after ",round(as.numeric(difftime(end_time,start_time,uni="mins")),2)," min"))
  ##########################################################
  ## fiND MINIMUM SUM OF SQUARES AND CREATE DATAFRAME
  ##########################################################

  minml <- which(ss==min(ss))

  message(paste("\n",Sys.time(),": Estimated: ",
                paste(names(TS.target),TS.target[minml,],sep=" ")))

  ss<- as.data.frame(ss)

  for(k in 1:n.nf){
    ss <- cbind(ss,TS.target[,k])
  }
  names(ss)<- c("ss",names(TS.target))

  ss<- ss[order(ss$ss),]
  ss$rank <- 1:nrow(ss)

  return(ss)
}
