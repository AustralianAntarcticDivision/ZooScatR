#' Save the parameter file
#' @description Save the model parameter. Keep standard dat format, as used in th eoriginal Matlab code, to keep maximum compatibility with previously generated files
#' @param para Parameters
#' @param fn Filename with path, to where the dat file should be written
#' @return Returns a a .dat file with all model parameters
#' @examples
#' fn <- 'config_0.dat'
#' para <- read_para(fn)
#' createParaDat(para,fn)


createParaDat <- function(para, fn){

		fileConn<-file(fn,'a')
				writeLines(c("%% home directory"), fileConn)
				writeLines(dirname(fn), fileConn)
				writeLines(c("%% shape parameters"), fileConn)
				writeLines(c(paste(sep="		",para$shape$L,"% length in mm")), fileConn)
				writeLines(c(paste(sep="		",para$shape$rho_L,"% rho/L")), fileConn)
				writeLines(c(paste(sep="		",para$shape$L_a,"% L/a")), fileConn)
				writeLines(c(paste(sep="		",para$shape$order,"% tapering order")), fileConn)
				writeLines(c(paste(sep="		",para$shape$ave_flag,"% length average flag: 0-no average,	1-average")), fileConn)
				writeLines(c(paste(sep="		",para$shape$Lstd,"% relative length standard deviation (meaningless for no average)")), fileConn)
				writeLines(c(paste(sep="		",para$shape$dL,"% relative length increment(meaningless for no average)")), fileConn)
				writeLines(c(paste(sep="		",para$shape$prof_name,"% shape profile name ( -1 if profile is not specified)")), fileConn)
				writeLines(c(paste(sep="		",para$shape$axis_sm,"% number of points for axis smoothing (meaningless if profile is not specified)")), fileConn)
				writeLines(c(paste(sep="		",para$shape$taper_sm,"% number of points for tapering function smoothing (meaningless if profile is not specified)")), fileConn)
				writeLines(c(paste(sep="		","%% orientation parameters")), fileConn)
				writeLines(c(paste(sep="		",para$orient$angm,"% mean incident angle (deg)")), fileConn)
				writeLines(c(paste(sep="		",para$orient$ang0,"% angle variation range - starting value (deg)")), fileConn)
				writeLines(c(paste(sep="		",para$orient$ang1,"% angle variation range - ending value (deg)")), fileConn)
				writeLines(c(paste(sep="		",para$orient$ave_flag,"% average option flag: 0-no average,	1-average")), fileConn)
				writeLines(c(paste(sep="		",para$orient$PDF,"% PDF flag: 1-Uniform		2-Gaussian")), fileConn)
				writeLines(c(paste(sep="		",para$orient$PDF_para,"% PDF parameter: half range for Uniform PDF,")), fileConn)
				writeLines(c(paste(sep="		","%","% standard deviation for Gaussian (deg)")), fileConn)
				writeLines(c(paste(sep="		","%","% (meaningless for no average)")), fileConn)
				writeLines(c(paste(sep="		",para$orient$dang,"% angle increment	(meaningless for no average)")), fileConn)
				writeLines(c(paste(sep="		","%% physical property parameters")), fileConn)
				writeLines(c(paste(sep="		",para$phy$g0,"% mean density contrast (g0)")), fileConn)
				writeLines(c(paste(sep="		",para$phy$h0,"% mean sound speed contrast (h0)")), fileConn)
				writeLines(c(paste(sep="		",para$phy$seg_no,"% number of body segments (1-homogeneous body)")), fileConn)
				writeLines(c(paste(sep="		",para$phy$g_std,"% standard deviation of g0 (meaningless for homogeneous body)")), fileConn)
				writeLines(c(paste(sep="		",para$phy$h_std,"% standard deviation of h0 (meaningless for homogeneous body)")), fileConn)
				writeLines(c(paste(sep="		",para$phy$corrL,"% correlation length as a percentage of body length L")), fileConn)
				writeLines(c(paste(sep="		","%% simulation parameters")), fileConn)
				writeLines(c(paste(sep="		",para$simu$ni,"% integration	points along body axis")), fileConn)
				writeLines(c(paste(sep="		",para$simu$n,"% number of output points")), fileConn)
				writeLines(c(paste(sep="		",para$simu$out_indx,"% output variable index: 1-scatting amplitude, 2-cross section, 3-TS, 4-RTS")), fileConn)
				writeLines(c(paste(sep="		",para$simu$var_indx,"% variable: 1-frequency (kHz), 2-ang(deg),		3-ka")), fileConn)
				writeLines(c(paste(sep="		",para$simu$var0,"% start value for the variable")), fileConn)
				writeLines(c(paste(sep="		",para$simu$var1,"% end value for the variable")), fileConn)
				writeLines(c(paste(sep="		",para$simu$freq,"% frequency (kHz), enabled for variable being angle only")), fileConn)
		close(fileConn)
}
