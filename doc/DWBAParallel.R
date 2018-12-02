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

## ------------------------------------------------------------------------
TS.df <- foreach(p=1:length(profiles), .combine=rbind) %:% #loop through profiles
   
  foreach(l = seq(lmin,lmax, by=lincr),.combine=rbind) %:% #loop through lengths
  
  foreach(theta = seq(t_min,t_max,by=tincr), #loop through orientations
          .combine=rbind, # make a rowbind of the results
          .packages=c("ZooScatR"), # bind ZooScatR to the loop
          .options.snow = opts) %dopar% { # uncomment to show progress bar (commented for vignette)
            para$shape$prof_name = profiles[p] #load a preset shape
            para$shape$L <- l #set length
            para$orient$angm <- theta #set orientation
            
            # Run DWBA based on config file (invisible to suppress warnings)
            invisible(capture.output(res <- bscat(para=para, misc=misc)))
            res.df <- as.data.frame(t(res$y)) #store results in a dataframe
            names(res.df) <- res$var #Set the names of the datraframe header
            res.df$id <- para$shape$prof_name #set the profile name as id
            res.df$L <- para$shape$L #add length information
            res.df$theta <- para$orient$angm #add orientation informaiton
            res.df
            }
message("Finished simulations...")

## ------------------------------------------------------------------------
close(pb) #Close progressbar and cluster
stopCluster(cl)

## ------------------------------------------------------------------------
message("Saving files")
write.csv(file=paste0(out_fn,".csv"), TS.df)
#Check if file was written successfully
if(file.exists(paste0(out_fn,".csv"))){ 
  message(paste0(paste0(out_fn,".csv"), " Written sucessfully!"))
}else{message(paste0("ERROR WRITING ", paste0(out_fn,".csv")))}

saveRDS(TS.df,paste0(out_fn,".RDS"))
#Check if file was written successfully
if(file.exists(paste0(out_fn,".RDS"))){
  message(paste0(paste0(out_fn,".RDS"), " Written sucessfully!"))
}else{message(paste0("ERROR WRITING ", paste0(out_fn,".RDS")))}


## ----fig.width=7,fig.height=4--------------------------------------------
ggts <- melt(TS.df, id=c("id","L","theta")) #melt data frame to be used in ggplot 

#create the plot settings
TS_raster <- function(f){
  pp<- ggplot(data=ggts[ggts$variable==as.character(f),],aes(fill=value, x=L,y=theta))+
    facet_grid(.~id)+ #make grid for multiple profiles
    geom_raster(interpolate=TRUE)+ #make raster plot
    scale_fill_viridis(name="TS [dB]", limits=c(-95,-60), oob = scales::squish)+ #use viridis color scheme
    scale_x_continuous(expand = c(0,0))+ #remove white space on x axis
    scale_y_continuous(expand = c(0,0))+ #remove white space on y axis
    xlab("Length [mm]")+ylab("Theta [degrees]")+ #modify x and y axis labels
    ggtitle(paste("TS @",f,"kHz" ))
    theme_minimal()+ #use minimal theme
    theme(legend.position="top", #place legend on top
          axis.text = element_text(size=14), #change font size on x and y axis
          axis.title = element_text(size=16)) #change axis title font size
  print(pp)
}  

#make plot at 38 kHz
TS_raster(38)
#make plot at 70 kHz
TS_raster(70)
#make plot at 120 kHz
TS_raster(120)
#make plot at 200 kHz
TS_raster(200)


## ----fig.width=7,fig.height=7--------------------------------------------
f.cols <- 1:(length(TS.df)-3) #get the number of output points
TS.df[,f.cols] <- 10^(TS.df[,f.cols]/10) #linearise TS
vv2 <- which(TS.df$L %in% seq(10,100,by=10) & TS.df$theta %in% c(seq(-90,90,by=30))) #create a selection of the variable  values to be plotted
TS <- melt(TS.df[vv2,-which(names(TS.df)=="id")], id.vars=c("L","theta")) #reshape the dataframe to be compatile with ggplot
TS$Frequency <- as.numeric(as.character(TS$variable)) #make sure frequency is a numeric value
TS$value <- 10*log10(TS$value)
TS$L <- paste0(TS$L, " mm")
TS$theta <- paste0(TS$theta, "°")

TS.plot <-ggplot(data=TS, aes(x=Frequency,y=value, fill='value'))+
  geom_path(size=1)+
  facet_grid(L~theta)+
  xlab("Frequency [kHz]")+ylab('TS [dB]')+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=16,angle=75,hjust=1),
        axis.title = element_text(size=20),
        strip.background = element_rect(color="black", fill="white", size=2, linetype=0),
        strip.text = element_text(size = 16))
TS.plot

