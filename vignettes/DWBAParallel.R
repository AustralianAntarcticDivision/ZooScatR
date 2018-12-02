## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(ZooScatR)
set.seed(1014)

## ----warning=FALSE-------------------------------------------------------
#function to check if packages are available, otherwise installs them and loads them
libcheck <- function(x){ 
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
#list of packages needed
libs = c("ZooScatR",
         "doSNOW", #for paralled process
         "tcltk2", #for os independent progressbar without RMarkdown output
         "ggplot2", #for plotting
         "viridis", #for nicer colors
         "reshape2") #reshaping the results dataframe to be used in ggplot
libcheck(libs)


## ------------------------------------------------------------------------
#Set filename for preset config file
fname = paste0(system.file(package="ZooScatR"),"/extdata/configs/config_krill.dat")
#Read configuration from file
para = read_para(fname) 
#Define the soundspeed in the surrounding fluid
misc <- list(cw = 1460) #soundspeed m/s

## ------------------------------------------------------------------------
#set length and theta limits
#minimum and maximum length [mm], increment step
lmin = 30; lmax = 60; lincr = 1

## ----fig.width=7,fig.height=3, warning=FALSE-----------------------------
#For a range of example profiles
profiles = paste0(paste0(system.file(package = "ZooScatR"),"/extdata/profiles/"),list.files(paste0(system.file(package = "ZooScatR"),"/extdata/profiles/"), pattern=".dat"))

#For this example we will only select a generic, hypothetical krill shape but set the parallel process to be able to cope with numerous profiles
profiles = paste0(paste0(system.file(package = "ZooScatR"),"/extdata/profiles/euphaus0.dat"))

#Check if the correct shape profile is selected:
para$shape$prof_name = profiles #Update the parameters (not necessary for the parallel process)
sprof = buildpos(para) #build the shape 
sprof$plot #Check the shape plot

## ------------------------------------------------------------------------
#minimum and maximum theta, increment step
t_min = -90; t_max = 0; tincr = 1

## ------------------------------------------------------------------------
startfreq = 14; endfreq = 400; fincr = 1

para$simu$var0 <- startfreq #set starting frequency
para$simu$var1 <- endfreq #set end frequency
para$simu$n <- length(seq(para$simu$var0,para$simu$var1,by=fincr)) #number of output points based on the frequency range

## ------------------------------------------------------------------------
#Total number of simulations
total <-length(seq(lmin,lmax,by=lincr))*
  length(seq(t_min,t_max,by=tincr))*
  length(profiles)
message(paste("The total number of simulations is:", total))

## ------------------------------------------------------------------------
pb <- tkProgressBar("ZooScatR - DWBA", "Initialising Model...", 0, total)
progress <- function(n) {
  info <- sprintf("Model progress: %d%% completed", round(n/total*100))
  setTkProgressBar(pb, n, sprintf("test (%s)", info), info)
  }
opts <- list(progress = progress)

## ------------------------------------------------------------------------
out_fn = "TS_sim"

## ------------------------------------------------------------------------
#Set number of cores to be used:
ncl = 8
#Initialise the cluster in Snow
cl <- makeCluster(ncl)
registerDoSNOW(cl)

