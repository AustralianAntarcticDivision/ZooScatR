## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(ZooScat)
set.seed(1014)

## ----fig.width=8,fig.height=4--------------------------------------------
fname <- paste0(system.file(package="ZooScat"),"/extdata/configs/config_0.dat") #Loacation of the parameters file
para = read_para(fname) #Read parameters file
#Create list with soundspeed info
misc <- list(cw=1500)
#Run DWBA based on config file
res <- bscat(para=para, misc=misc) #Target strength vs Frequency
res$rplot #Show the result plot
#Target strength vs angular position
para$simu$var_indx <- 2 #Set output variable to Angular position
#Run DWBA based on config file
res <- bscat(para=para, misc=misc)
res$rplot #Show the result plot
#Backscattering amplitude vs ka (Wavenumber x width)
para$simu$out_indx <- 1 #Set output to Backscattering amplitude
para$simu$var_indx <- 3 #Set output variable to ka
#Run DWBA based on config file
res <- bscat(para=para, misc=misc)
res$rplot #Show the result plot
#Reduced Target Strength vs Frequency (Wavenumber x width)
para$simu$out_indx <- 4 #Set output to Reduced Target Strength
para$simu$var_indx <- 1 #Set output variable to Frequency
#Run DWBA based on config file
res <- bscat(para=para, misc=misc)
res$rplot #Show the result plot

## ------------------------------------------------------------------------
# filenmae of an example configuration
fname <- paste0(system.file(package="ZooScat"),"/extdata/configs/config_0.dat")
#Read the parameter file
para <- read_para(fname)

## ----echo=FALSE----------------------------------------------------------
para.df <- as.data.frame(cbind(Variable = c("Length(L)",
                                      "<U+03C1>c/L",
                                      "L/a",
                                      "Taper order",
                                      "Length average",
                                      "std(L)/L",
                                      "Length increment",
                                      "Shape profile",
                                      "Taper smooth",
                                      "Axis smooth"),
                         Unit = c("mm",
                                  "ratio",
                                  "ratio",
                                  "float",
                                  "boolean",
                                  "mm",
                                  "mm",
                                  "filepath",
                                  "float",
                                  "float"),
                         Description = c("The total target body length",
                                         "Ratio of the radius of curvature (<U+03C1>c) and L",
                                         "L/a with a the radius of the mid-point of the cylinder is the ratio of L to a",
                                         "The taper order (n) controls the tapering.",
                                         
                                         "The average model over a range of lengths will be produced",
                                         "The ratio of the standard deviation of length (std(L)) to the mean length (L)",
                                         "This value has to be chosen with care, as it has to be possible to calculate the provided std(L)/mean(L) around the provided mean L with the given increment.",
                                         "Path to a profile file containing 3 or 5 columns. (X, Z, R coordinates and optional Xupper Xlower). The profile file must contain at least 3 columns, containing the center coordinates (X, Z, R) of each circular segment",
                                         "Filter length to smooth the radius (R)",
                                         "Filter length to smooth the body axis (X, Z)"),
                         Influence = c("Length of the target",
                                       "Curvature of the bent cylinder, if no shape profile is loaded ( 8 for straight cylinder)",
                                       "Contains information about the width of the target.",
                                       "For a straight cylinder (<U+03C1>c/L -> infinity), n=2 results in a prolate spheroid if L>2a, or in an oblate spheroid for L<2a",
                                       "1 = perform average; 0 = No average",
                                       "Defines the variability of the target lengths, used to compute the input models for the averaged model output",
                                       "Defines the length increment of the target lengths, used to compute the input models for the averaged model output",
                                       "Defines the shape of the target, if no profile is selected a uniformly bent cylinder will be computed",
                                       "1 = no smoothing, otherwise smoothes the tapering (reduces complexity)",
                                       "1 = no smoothing, otherwise smoothens the axis (flatter)"),
                         "Parameters name" =
                           c("...shape$L",
                             "...shape$rho_L",
                             "...shape$L_a",
                             "...shape$order",
                             "",
                             "...shape$Lstd",
                             "...shape$dL",
                             "...shape$prof_name",
                             "...shape$taper_sm",
                             "...shape$axis_sm")
                         ))
knitr::kable(para.df)

## ----echo=FALSE----------------------------------------------------------

orient.df <- as.data.frame(cbind(Variable = c("Mean Theta (<U+03B8>)",
                                      "Orientation Average",
                                      "Theta Distribution (d<U+03B8>)",
                                      "Min. Theta (<U+03B8>min)",
                                      "Max. Theta (<U+03B8>max)",
                                      "Std.(<U+03B8>)",
                                      "Increment <U+03B8>"),
                         Unit = c("°",
                                  "Boolean",
                                  "distribution (Uniform or Gaussian)",
                                  "°",
                                  "°",
                                  "°",
                                  "°"),
                         Description = c("Mean orientation angle",
                                         "Defines if an orientation average should be considered",
                                         "PDF of theta",
                                         "Minimum theta for a uniform PDF",
                                         
                                         "Maximum theta for a uniform PDF",
                                         "Standard deviation of theta for a Gaussian PDF",
                                         "Incremental step of theta for a Gaussian PDF"),
                         Influence = c("",
                                       "",
                                       "1) Uniform; 2) Gaussian","","","",""),
                         "Parameters name" =
                           c("...orient$angm",
                             "...orient$ave_flag",
                             "...orient$PDF",
                             "...orient$ang0",
                             "...orient$ang1",
                             "...orient$PDF_para",
                             "...orient$dang")
                         ))
knitr::kable(orient.df, keep.line.breaks = TRUE, style = 'grid', justify = 'left')

## ----echo=FALSE----------------------------------------------------------

mat.df <- as.data.frame(cbind(Variable = c("Density contrast (g)",
                                      "Soundspeed contrast (h)",
                                      "Ambient Soundspeed (c)",
                                      "N body segment",
                                      "std(g)",
                                      "std(h)",
                                      "Correlation Length"),
                         Unit = c("ratio",
                                  "ratio",
                                  "m/s",
                                  "Integer",
                                  "std of ratio",
                                  "std of ratio",
                                  "% of L"),
                         Description = c("the ratio of the density of the animal to the density of the surrounding fluid",
                                         "the ratio of the sound speed in the animal to the sound speed in the surrounding fluid",
                                         "Soundspeed in surrounding fluid",
                                         "Defines the number of segments with different gi and hi along the body axis (>1 if an inhomogenous body should be computed).",
                                         
                                         "Standard deviation of the density (g), if an inhomogenous body should be computed",
                                         "Standard deviation of the sound speed contrast (h), if an inhomogenous body should be computed",
                                         "Controls the variability of g and h along the body axis (only xonsidered if the number of segments is >8)"),
                         Influence = c("Influences acoustic impedance, should be within [0.95, 1.05]",
                                       "Influences acoustic impedance, should be within [0.95, 1.05]",
                                       "","","","",""),
                         "Parameters name" =
                           c("...phy$g0",
                             "...phy$h0",
                             "...misc$cw",
                             "...phy$seg_no",
                             "...phy$g_std",
                             "...phy$h_std",
                             "...phy$corrL")
                         ))
knitr::kable(mat.df, keep.line.breaks = TRUE, style = 'grid', justify = 'left')

## ----echo=FALSE----------------------------------------------------------

simu.df <- as.data.frame(cbind(Variable = c("Output",
                                      "Variable",
                                      "Sample points",
                                      "Integration Points",
                                      "Variable start value",
                                      "Variable end value",
                                      "Frequency"),
                         Unit = c("Selection",
                                  "Selection",
                                  "Integer",
                                  "Integer",
                                  "Float",
                                  "Float",
                                  "Float"),
                         Description = c("Model variable",
                                         "Number of output points along the variable maximum and minimum value",
                                         "Number of integration points along the target body",
                                         "Resolution of the model variable output",
                                         "Minimum output value",
                                         
                                         "Maximum output value",
                                         "Defines the discrete frequency for which the model is run if the variable is set to angle"),
                         Influence = c("1) backscattering amplitude; 2) differential backscattering cross-section [m2]; 3) Reduced Target Strength [dB re m2]; 4) Reduced Target Strength [db re m2]",
                                         "1) Frequency [kHz]; 2) Angle (°); 3) ka (wave number k * a)",
                                         "Resolution of the model variable output",
                                         "Resolution of the model shape input",
                                         
                                         "Range of the mdel output variable",
                                         "Range of the mdel output variable",
                                         "Center frequency of model variation around a range of theta"),
                         "Parameters name" =
                           c("...simu$out_indx",
                             "...simu$var_indx",
                             "...simu$n",
                             "...simu$ni",
                             "...simu$var0",
                             "...simu$var1",
                             "...simu$freq")
                         ))
knitr::kable(simu.df, keep.line.breaks = TRUE, style = 'grid', justify = 'left')

## ----echo=TRUE-----------------------------------------------------------
#Changing the length of the scattering target
message(paste("Old length:", para$shape$L))
para$shape$L <- 40
message(paste("New length:", para$shape$L))
#Changing the mean orientation of the scattering target
message(paste("Old orientation angle:", para$orient$angm))
para$orient$angm <- 30
message(paste("New orientation angle:", para$orient$angm))
#Changing the density contrast
message(paste("Old density contrast:", para$phy$g0))
para$phy$g0 <- 1.031
message(paste("New density contrast:", para$phy$g0))
#Changing the output start value
message(paste("Old output start value:", para$simu$var0))
para$simu$var0 <- 38
message(paste("New output start value:", para$simu$var0))
#Changing the output end value
message(paste("Old output end value:", para$simu$var1))
para$simu$var0 <- 200
message(paste("New output end value:", para$simu$var1))

## ----fig.width=6,fig.height=3--------------------------------------------
cyl <- buildpos(para)
message(paste("Length:",para$shape$L))
message(paste("L/a:",para$shape$L_a))
message(paste("Tapering order:",para$shape$order))
cyl$plot

para$shape$order <- 200
para$shape$L_a <- 3
cyl <- buildpos(para)
message(paste("Length:",para$shape$L))
message(paste("L/a:",para$shape$L_a))
message(paste("Tapering order:",para$shape$order))
cyl$plot

para$shape$order <- 2
para$shape$L_a <- 2
para$shape$rhoL <- 200
cyl <- buildpos(para)
message(paste("Length:",para$shape$L))
message(paste("L/a:",para$shape$L_a))
message(paste("Tapering order:",para$shape$order))
cyl$plot

## ----fig.width=8,fig.height=4--------------------------------------------
# filenmae of an example configuration
fname <- paste0(system.file(package="ZooScat"),"/extdata/configs/config_0.dat")
#Read the parameter file
para <- read_para(fname)
#set the profile filename
profname <- paste0(system.file(package="ZooScat"),"/extdata/profiles/euphaus1.dat") #krill example
para$shape$prof_name <- profname
krill_shape1 <- buildpos(para)
krill_shape1$plot

#change the taper smoothing to some smoothing
para$shape$taper_sm <- 100
krill_shape1 <- buildpos(para)
krill_shape1$plot

#change the axis smoothing, taper smoothing to some extreme values and L/a to obtian a more slender body
para$shape$L_a <- 30
para$shape$taper_sm <- 2000
para$shape$axis_sm <- 2000
krill_shape1 <- buildpos(para, disp_prof=1)
krill_shape1$plot

## ---- echo=FALSE---------------------------------------------------------
taxa <- c("Euphausiids and Decapod Shrimp",
          "Larval Crustacean",
          "Amphipods",
          "Ostracods",
          "Chaetognaths and Polychaetes",
          "Gymnosome Pteropods (Clione)",
          "Salps",
          "Copepods",
          "Medusae",
          "Eggs",
          "Calanus finmarchicus",
          "Calanus hyperboreus",
          "Acartia clausi",
          "Calanus marshallae",
          "Meganyctiphanes norvegica",
          "Thysanoessa",
          "Calanus",
          "General",
          "Euphausia superba",
          "Euphausia crystallorophias",
          "Neocalanus cristatus",
          "Neocalanus plumchrus",
          "Ammodytes personatus (juvenile)",
          "Ammodytes personatus (adult)",
          "Salpa thompsoni")

La <- c("10.5",
        "2.55 [@lawson_acoustically-inferred_2004]",
        "3.00 [@lawson_acoustically-inferred_2004]",
        2.55,
        17.15,
        1.83,
        4.0,
        2.55,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,NA,NA,NA,NA,10)

Orientation <- c("N(20,20) [@benfield_estimating_2000], average ~0 [@lawson_improved_2006]",
                 "N(0.30) [@lawson_acoustically-inferred_2004]",
                 "N(0,30) [@lawson_acoustically-inferred_2004]",
                 "N(0,30)",
                 "N(0,30)",
                 "N(0,30)",
                 "N(0,30)",
                 "N(90,30)",
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,"Uniform [0, p]")


g <- c("$$\\frac{5.485*L}{10^4}+1.002; L>25$$ $$1.016; L<25$$ [@lawson_acoustically-inferred_2004]",
       "1.058 [@lawson_acoustically-inferred_2004]",
       "1.058 [@lawson_acoustically-inferred_2004]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.004 [@stanton_acoustic_1994]",
       "1.02 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.02 [inferred from @monger_sound_1998 as reported in @lavery_determining_2007]",
       "0.979 [@chu_material_2003]",
       "1.025 - 1.029  g/cm^3^ depending on season [@kogeler_density-and_1987]",
       "1.022 - 1.036  g/cm^3^ depending on season [@kogeler_density-and_1987]",
       "1.04 g/cm^3^ [@greenlaw_acoustical_1979]",
       "1.04 g/cm^3^ [@greenlaw_acoustical_1979]",
       "1.057 g/cm^3^ [@kils_swimming_1981]",
       "1.052 - 1.074 g/cm^3^ depending on season and species [@kogeler_density-and_1987]",
       "1.022 - 1.036 g/cm^3^ depending on season and species [@kogeler_density-and_1987]",
       "0.9402 - 1.051 [@chu_measurements_2005]",
       "1.0241 [@chu_measurements_2005], 1.0357 [@foote_speed_1990]",
       "1.009 & 1.000 depth dependent [@chu_measurements_2005]",
       "0.997 - 1.009 [@matsukura_measurements_2009]",
       "0.995 - 1.009 [@matsukura_measurements_2009]",
       "1.021 [@yasuma_density_2009]",
       "1.032 [@yasuma_density_2009]",
       "1 - 1.0039 [@wiebe_acoustic_2009]"
       )



h <- c("$$\\frac{5.94*2L}{10^ 4}; L>25$$ $$1.019; L<25$$  [@lawson_acoustically-inferred_2004]",
       "1.058 [@lawson_acoustically-inferred_2004]",
       "1.058 [@lawson_acoustically-inferred_2004]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.03 [pers. comm. Dezhang Chu in @lavery_determining_2007]",
       "1.004 [@stanton_acoustic_1994]",
       "1.058 [@chu_inference_2000]",
       "1.02 [inferred from @monger_sound_1998 as reported in @lavery_determining_2007]",
       "1.017 [@stanton_simple_1989]",
       "1.027 (mixture C. finmarchicus and C. hyperboreus) [@kogeler_density-and_1987]",
       "1.027 (mixture C. finmarchicus and C. hyperboreus) [@kogeler_density-and_1987]",
       "NA",
       "1.007 [@kogeler_density-and_1987]",
       "1.030 [@kogeler_density-and_1987]",
       "1.026 [@kogeler_density-and_1987]",
       "1.027 [@kogeler_density-and_1987];  0.949 & 1.013 - Depth dependent [@chu_measurements_2005]",
       "0.949 - 1.096 [@chu_measurements_2005]",
       "1.031 [@chu_measurements_2005], 1.0279 [@foote_speed_1990]",
       "1.025 & 1.029 depth dependent [@chu_measurements_2005]",
       "1.013 - 1.025 [@matsukura_measurements_2009]",
       "1.006 - 1.021 [@matsukura_measurements_2009]",
       "1.02 [@yasuma_density_2009]",
       "1.02 [@yasuma_density_2009]",
       "1.006 - 1.0201 [@wiebe_acoustic_2009]")


model <- c("DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA uniformly-bent cylinder","DWBA prolate spheroid", "DWBA two prolate spheroid surfaces", "High-pass fluid sphere","NA","NA","NA","NA","NA","NA","NA",NA,NA,NA,"DWBA Deformed Cylinder","DWBA Deformed Cylinder",NA,NA,"DWBA")

gh <- as.data.frame(cbind(taxa,La,Orientation,g,h,model))
names(gh)<- c("Taxon",
              "Length-to-girth ratio $\\frac{L}{a}$",
              "Orientation",
              "Density Contrast g", 
              "Sound Speed Contrast h",
              "Scattering model")
knitr::kable(gh)
#datatable(gh)

## ------------------------------------------------------------------------
c_Coppens1981(D=100,S=35,T=10)

## ------------------------------------------------------------------------
c_Mackenzie1981(D=100,S=35,T=10)

## ------------------------------------------------------------------------
c_Leroy08(Z=100,T=10,S=35,30)

## ------------------------------------------------------------------------
rho(S=35, T=10)
#which is equal to:
rho_p0(S=35,T=10)

## ------------------------------------------------------------------------
rho_smow(T=10)

## ------------------------------------------------------------------------
result <- bscat(para=para, misc=misc)

