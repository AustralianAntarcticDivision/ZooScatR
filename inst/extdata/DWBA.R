
####Minimal example
library(ZooScat)
#### Run based on config file and command based
#Set filename ro config file
fname = "/Users/sveng/Documents/Tank/GUI_DWBA_scat_model/mfiles/configuation/config_0.dat"
#read config file
para = read_para(fname)
#Create list with soundspeed info
misc <- list()
misc$cw <- 1500
#Run DWBA based on config file

para$phy$body_ih = TRUE
para$phy$g_std <- 0.008
para$phy$h_std <- 0.008

para$orient$ave_flag<-0
para$shape$ave_flag<-0

para$simu$out_indx<-3

res <- bscat(para=para, misc=misc, app=FALSE)
res$rplot

### Run shiny app
