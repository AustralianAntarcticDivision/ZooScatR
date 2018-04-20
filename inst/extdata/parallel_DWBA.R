#' Run DWBA in parallel
#' needs to be wrapped into function and needs to be run through roxygen
#' @import doParallel
#' @import foreach
#' @import pracma
#' @import doSNOW
#' @param para Model Parameters
#' @return list with all parameters for DWBA
#' @examples
#'

####################################################################
#### Run based on config file with customised settings in parallel
library(ZooScat)
library(doSNOW)
#Set number of cores
ncl = 8

#Set filename for preset config file
fname = paste0(system.file(package="ZooScat"),"/extdata/configs/config_0.dat")

###Settings

#Soundspeed
cw =1460

## Shape parameters
#set length and theta limits
#minimum length [mm]
lmin =30
#maximum length[mm]
lmax=60
#length increment step
lincr = 1

#min theta
t_min =-90
#max theta
t_max= 0
#theta increment step
tincr = 1

#smoothing
#axis smoothin
s_sm <- 0
#taper smoothing
t_sm <- 0

#Shape profile filename
profiles = paste0(system.file(package = "ZooScat"),"/extdata/profiles/",list.files(path="./extdata/profiles/",pattern=".dat")[3:4])

para = read_para(fname) #Read configuration from file

## Simulation
# Frequencies
startfreq = 90
endfreq = 170
para$simu$var0 <- startfreq #set starting frequency
para$simu$var1 <- endfreq #set end frequency

para$simu$n <- length(seq(para$simu$var0,para$simu$var1,by=0.1))
para$orient$ave_flag <- -1 #Orientation Average - 1 for yes

#Create list with soundspeed info
misc <- list()
misc$cw <- cw

#Set output filename (saved as csv and rds)
out_fn = "TS_sim"
###################################################################

#index
index=1

#Total number of simulations
total <-length(seq(lmin,lmax,by=lincr))*length(seq(t_min,t_max,by=tincr))*length(profiles)*length(s_sm)*length(t_sm)
#Set progressbar
pb <- txtProgressBar(min = index-1, max = total, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

#Run loop in parallel
message("Initialising cluster....")
cl <- makeCluster(ncl)
registerDoSNOW(cl)

message("Starting Loop...")
TS.df <- foreach(p=1:length(profiles), .combine=rbind) %:%
  foreach(l = seq(lmin,lmax, by=lincr),.combine=rbind) %:%
  foreach(ssm = s_sm,.combine=rbind) %:%
  foreach(tsm = t_sm,.combine=rbind) %:%
    foreach(theta = seq(t_min,t_max,by=tincr),
            .combine=rbind,
            .packages=c("ZooScat"),
            .options.snow = opts) %dopar% {
            #.verbose=T) %dopar% {
      #set profile
      para$shape$prof_name = profiles[p] #load a preset shape
      #set sampling

      para$shape$L <- l
      para$shape$ave_flag <- -1 #Length Average - 1 for yes

      para$shape$axis_sm<-ssm
      para$shape$taper_sm<-tsm

      para$orient$angm <- theta

      #Run DWBA based on config file
      #invisible(capture.output(res <- bscat(para=para, misc=misc, status=status)))
      res <- bscat(para=para, misc=misc)
      #res$rplot
      res.df <- as.data.frame(t(res$y))
      names(res.df) <- res$var

      res.df$id <- para$shape$prof_name
      res.df$L <- para$shape$L
      res.df$theta <- para$orient$angm
      #index <- index+1
      res.df
            }
message("Finished loop, stopping cluster")
#CLose progressbar and cluster
close(pb)
stopCluster(cl)
message("Saving files")

#save
write.csv(file=paste0(out_fn,".csv"), TS.df)
if(file.exists(paste0(out_fn,".csv"))){
  message(paste0(paste0(out_fn,".csv"), " Written sucessfully!"))
}else{message(paste0("ERROR WRITING ", paste0(out_fn,".csv")))}

saveRDS(TS.df,paste0(out_fn,".RDS"))
if(file.exists(paste0(out_fn,".RDS"))){
  message(paste0(paste0(out_fn,".RDS"), " Written sucessfully!"))
}else{message(paste0("ERROR WRITING ", paste0(out_fn,".RDS")))}
