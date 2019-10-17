## ----warning=FALSE-------------------------------------------------------
library(ZooScatR)

## ----eval=FALSE----------------------------------------------------------
#  DWBAapp()

## ----echo=FALSE----------------------------------------------------------
para.df <- as.data.frame(cbind(Variable = c("Length(L)",
                                      "$\\rho_c$/L",
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
                                         "Ratio of the radius of curvature (Ïc) and L",
                                         "L/a with a the radius of the mid-point of the cylinder is the ratio of L to a",
                                         "The taper order (n) controls the tapering.",
                                         
                                         "The average model over a range of lengths will be produced",
                                         "The ratio of the standard deviation of length (std(L)) to the mean length (L)",
                                         "This value has to be chosen with care, as it has to be possible to calculate the provided std(L)/mean(L) around the provided mean L with the given increment.",
                                         "Path to a profile file containing 3 or 5 columns. (X, Z, R coordinates and optional Xupper Xlower). The profile file must contain at least 3 columns, containing the center coordinates (X, Z, R) of each circular segment",
                                         "Filter length to smooth the radius (R)",
                                         "Filter length to smooth the body axis (X, Z)"),
                         Influence = c("Length of the target",
                                       "Curvature of the bent cylinder, if no shape profile is loaded ( â<U+0088><U+009E> for straight cylinder)",
                                       "Contains information about the width of the target.",
                                       "For a straight cylinder (Ïc/L -> infinity), n=2 results in a prolate spheroid if L>2a, or in an oblate spheroid for L<2a",
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

orient.df <- as.data.frame(cbind(Variable = c("Mean Theta ($\\theta$)",
                                      "Orientation Average",
                                      "Theta Distribution (d$\\theta$)",
                                      "Min. Theta ($\\theta_{min}$)",
                                      "Max. Theta ($\\theta_{max}$)",
                                      "Std.($\\theta$)",
                                      "Increment $\\theta$"),
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
                                         "1) Frequency [kHz]; 2) Angle (Â°); 3) ka (wave number k * a)",
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

