#' DWBA shiny app
#' @description This is a Shiny web application for the DWBA model.
#'
#' @section General outputs:
#'
#'
#' All Shape, Orientation, Material Property and Simulation settings that are used as model input can be changed manually or be defined through configuration and profile files.
#'
#' Whenever the model is run, two plots will be generated and a data table will be generated.
#'
#' @section Output plots:
#' Two plots will be displayed after each model run.
#' One plot is displaying the shape of the input object and a second one showing the model output versus a selected variable. Model outputs and display variable can be selected under the Simulation Tab.
#'
#' \emph{Exporting plots}
#'
#' Plots can be exported by right-clicking the plot and selecting save as. The size of the plots will automatically adopt to the size of the browser window.
#' The values of the plot can be eplored by hovering over the plot with the mouse. The value closest to the cursor location and the distance to the closest value point are being displayed below the plot.
#' \emph{Zooming in the model output plot}
#'
#' Zooming can be achieved by drawing a box (holding left mouse key down) on the output plot. Once the box is visible, the plot will zoom into the selected area through double-clicking. Returning to the original zoom can be achieved through double clicking on the plot outside of the box area.
#'
#' @section Datatable output:
#' The datatable can be exported as csv file by clicking the Export button above the shape plot. The number of shown datapoints per table page can be varied by chaning the Show entries option box.
#'
#' @section Model parameters:
#' Four different types of model input parameters an be defined:
#' \enumerate{
#'       \item Shape
#'       \item Orientation
#'       \item Orientation
#'       \item Simulation
#'    }
#'
#' These different input types are organised in tabs.
#' Predefined configuration files which can define all settings can be loaded by clicking the Browse button under the Load Config menu at the top of the right frame.
#'
#' \strong{Shape settings:}
#'
#' Arbitrary shapes can be loaded by clicking the Shape Profile checkbox and loading a .dat file containing at least three columns without headers. The first column contains x coordinates, the second column contains the y coordinates and the thrid column contains the tapering for each point describing the shape.
#' The length (in mmm) and tapering of the shape can be defined through numeric inputs. The tapering of the shape and the x axis can be smoothed. Increasing the axis smoothing flattens the shape. Increasing the taper smooth smoothens (simplifies) the shape. If the taper smooth is set to above 0, the dashed line on the shape plot will represent the original input coordinates, while the solid line represents the smoothed, model input shape.
#'
#' If no shape profile is loaded, a uniformely bent cylinder can be generated with Length, radius of curvature girth (L/a) and tapering order information. If the radius of curvature is unknown, it can be computed for a given set of points by using
#'
#' If the model should be run over a range of lengths, a length average can be computed. If a length average should be calculated, the standard deviation and the increment need to be fixed.
#'
#' Changing the length
#' \strong{Orientation settings:}
#'
#' On the orientation tab, the mean, minimum and maximum orientation angles (theta) can be defined. An average result over a range of orientation angles can be computed, following either a gaussian or uniform distribtuion. If an average should be calculated, the standard deviation and increment have to be fixed.
#'
#' \strong{Material Property settings:}
#'
#'
#' \strong{Simulation settings:}
#'
#'#' Here the settings for the simulation and output plot settings are defined:
#'
#' \emph{Plot Output options are:}
#' TS, Scattering Amplitude, Cross-Section and reduced TS
#'
#' \emph{Variable Options are:}
#' Frequency [kHz], angle [degrees] and ka (wave number)
#'
#' @import shiny
#' @import shinyjs
#' @import ggplot2
#' @import reshape2
#' @import pracma
#' @return Runs a DWBA web application
#' @examples
#' DWBAapp() #run the web applications


DWBAapp <- function(){
  #library(shiny)
  #library(shinyjs)
  #library(ggplot2)

  shiny::shinyApp(
    ui <- shiny::fluidPage(
      # Load Shinyjs to show/hide tabs easily
      shinyjs::useShinyjs(),
      # Set CSS in addition to standard bootstrap
      shiny::tags$style(shiny::HTML("hr {
                              display: block;
                              margin-top: 0.5em;
                              margin-bottom: 0.5em;
                              margin-left: auto;
                              margin-right: auto;
                              border-color: dark-gray;
                              border-style: inset;
                              border-width: 1px;
                              }
                             a:link {
                                    color: #181818;
                                }

                                /* visited link */
                                a:visited {
                                    color: #787878;
                                }

                                /* mouse over link */
                                a:hover {
                                    color: #606060;
                                }"
                             )
                        ),

      # Application title
      shiny::titlePanel("ZooScat",

                 title=shiny::tags$div(
                   shiny::tags$img(src="extdata/ZOOSCAT.png",
                                   width=100,
                                   height=40), "ZooScat - Zooplankton Backscatter")
      ),
      # Sidebar
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::tabsetPanel(
            ### Shape panel
            shiny::tabPanel("Shape",
                            shiny::br(),
                     # Length input
                     shiny::tags$div(title="Length of the target [mm]",
                                     shiny::numericInput("l_inp",
                                     shiny::actionLink("l_info","Length L [mm]:"),
                                     min = 0.1,
                                     max = 100000,
                                     value = 30,
                                     step=0.1)),

                     # Curvature input
                     shiny::tags$div(title="radius of curvature / length",
                              shiny::numericInput("rho_inp",
                                           shiny::actionLink("rhoL_info",
                                                             shiny::HTML("&rho;<sub>c</sub>/L
                                                                          with &rho;<sub>c</sub> = radius of curvature of uniformly bent cylinder:")),
                                           min = 0.001,
                                           max = 100000,
                                           value = 3,
                                           step=0.01)),

                     # Length/area input
                     shiny::tags$div(title="Shape ratio defined as length [mm] / girth [m]",
                              shiny::numericInput("la_inp",
                                           shiny::actionLink("la_info","L/a, with a=radius of the midpoint of the cylinder:"),
                                           min = 0.1,
                                           max = 100000,
                                           value = 16,
                                           step=0.1)),

                     # Taper order input
                     shiny::tags$div(title="Tapering, the width of the target",

                              shiny::numericInput("taper_inp",
                                           shiny::actionLink("taper_info", "Taper order:"),
                                           min = 0.1,
                                           max = 100000,
                                           value = 10,
                                           step=0.1)),

                     shiny::tags$hr(),
                     # Length average checkbox
                     shiny::tags$div(title="Compute average over length range?",
                     shiny::checkboxInput("l_avg",
                                          shiny::actionLink("lav_info", "Length Average"),
                                          0)),
                     # Std Length
                     shiny::tags$div(title="Ratio Standard deviation of length = L to the mean(L) is only considered if the Length Average checkbox is ticked",
                              shiny::numericInput("stdll",
                                           shiny::actionLink("lav_info2",
                                                             shiny::HTML("std(L)/<span style='text-decoration:overline;'>L</span>:")),
                                           min = 0.1,
                                           max = 100000,
                                           value = 0.1,
                                           step=0.1)),
                     # L increment
                     shiny::tags$div(title="Increment for length range, only considered if average length should be computed, only considered if the Length Average checkbox is ticked",
                              shiny::numericInput("l_incr",
                                           shiny::actionLink("linc_info","Length increment:"),
                                           min = 0.001,
                                           max = 100000,
                                           value = 0.01,
                                           step=0.01)),

                     shiny::br(),
                     #profile checkbox
                     shiny::tags$div(title="Generate shape based on predefined coordinates?",
                              shiny::checkboxInput("profile_cb",
                                            shiny::actionLink("prof_info","Shape Profile"),
                                            0)),
                     #load profile browse
                     shiny::tags$div(title="Select a shape profile .dat file, which contains X,Z and radius R coordinates, only considered if the Shape Profile checkbox is ticked",
                              shiny::fileInput("prof_fn",
                                        "Choose a profile.dat File",
                                        accept = c(".dat"))),
                     #tapering smooth
                     shiny::tags$div(title="Smooth tapering of the loaded shape, only considered if the Shape Profile checkbox is ticked",
                              shiny::numericInput("tap_sm",
                                           shiny::actionLink("tapsm_info","Taper Smooth"),
                                           min=0,
                                           max=99999999,
                                           value=0,
                                           step=1)),
                     #axis smooth
                     shiny::tags$div(title="Smooth axis of the loaded shape, only considered if the Shape Profile checkbox is ticked",
                              shiny::numericInput("ax_sm",
                                           shiny::actionLink("axsm_info","Axis Smooth"),
                                           min=0,
                                           max=99999999,
                                           value=0,
                                           step=1))
            ),

            ### orientation panel
            shiny::tabPanel(
              title = "Orientation",
              # Mean theta
              shiny::br(),
              shiny::tags$div(title="Mean tilt angle in degrees",
                       shiny::numericInput("theta_avg",
                                    shiny::actionLink("mtheta_info",
                                               shiny::HTML("Mean Theta (<span style='text-decoration:overline;'>&theta;</span>) [°]:")),
                                    min = -360,
                                    max = 360,
                                    value = 0,
                                    step=0.1)),
              shiny::hr(),
              # Orientation average flag
              shiny::tags$div(title="Average over a range of tilt angles, following a distribution?",
                       shiny::checkboxInput("theta_flag",
                                     shiny::actionLink("avtheta_info", "Orientation Average"),
                                     0)),


              # Theta shape
              shiny::tags$div(title="Average over a range of tilt angles following a gaussian distribution. This will only be considered if the orientation average checkbox is ticked",
                       shiny::radioButtons("theta_distr",
                                    shiny::actionLink("tdist_info",
                                               shiny::HTML("Theta Distribution (d<sub>&theta;</sub>)")),
                                    c("Uniform PDF","Gaussian PDF"),"Gaussian PDF")),
              # Min Theta
              shiny::tags$div(title="Minimum tilt angle in degrees",
                       shiny::numericInput("theta_min",
                                    shiny::actionLink("mintheta_info",
                                               shiny::HTML("Min. Theta (&theta;<sub>min</sub>) [°]:")),
                                    min = -360,
                                    max = 360,
                                    value = 0,
                                    step=0.1)),
              # Max Theta
              shiny::tags$div(title="Maximum tilt angle in degrees",
                       shiny::numericInput("theta_max",
                                    shiny::actionLink("maxtheta_info",
                                               shiny::HTML("Max. Theta (&theta;<sub>max</sub>)[°]:")),
                                    min = -360,
                                    max = 360,
                                    value = 100,
                                    step=0.1)),
              # std Theta
              shiny::tags$div(title="Standard deviation of the tilt angle distribution, following a gaussian distribution. This will only be considered if the orientation average checkbox is ticked",
                       shiny::numericInput("theta_sd",
                                    shiny::actionLink("sdtheta_info",
                                                      shiny::HTML("Std.(&theta;) [°]:")),
                                    min = 0,
                                    max = 10000,
                                    value = 20,
                                    step=0.1)),
              shiny::tags$div(title="Increment step of the tilt angle over a range of tilt angles following either a uniform or gaussian distribution, this will only be considered if the orientation average checkbox is ticked",
                       shiny::numericInput("theta_inc",
                                    shiny::actionLink("inctheta_info",
                                                      shiny::HTML("Increment &theta; [°]:")),
                                    min = 0,
                                    max = 10000,
                                    value = 1,
                                    step=0.1))
            ),
            shiny::tabPanel(
              title="Material Property",
              # Density contrast
              shiny::br(),
              shiny::actionLink("gbut", shiny::HTML("<font color=	#404040><i>Show/hide some reported material properties</i></font>")),
              shiny::br(),
              shiny::br(),
              shiny::numericInput("g",
                           shiny::actionLink("g_info","Density contrast (g):"),
                           min = 1,
                           max = 1000,
                           value = 1.0357,
                           step=0.0001),
              shiny::actionLink("rhosw_but",
                                shiny::HTML("<font color=	#404040><i>Calculate seawater density based on Salinity, Temperature and pressure</i></font>")),
              shiny::br(),
              # Soundspeed contrast
              shiny::numericInput("h",
                           shiny::actionLink("h_info","Soundspeed contrast (h):"),
                           min = 0,
                           max = 1000,
                           value = 1.0279,
                           step=0.0001),
              shiny::numericInput("c",
                           shiny::actionLink("c_info","Ambient Soundspeed (c) [m/s]:"),
                           min = 0,
                           max = 2000,
                           value = 1460,
                           step=.1),
              shiny::actionLink("c_but",
                         shiny::HTML("<font color=	#404040><i>Calculate c based on Salinity, Temperature and Depth</i></font>")),
              shiny::hr(),
              # Inhomogenous body
              shiny::checkboxInput("body_ih",
                            shiny::actionLink("ih_info","Inhomogenous body"),
                            0),

              # N Body segemnts
              shiny::numericInput("n_seg",
                           shiny::actionLink("nseg_info","N body segments:"),
                           min = 1,
                           max = 100,
                           value = 7,
                           step=1),
              # std Density contrast
              shiny::numericInput("std_g",
                           shiny::actionLink("sdg_info","std(g):"),
                           min = 0,
                           max = 100,
                           value = 0.0016,
                           step=0.0001),
              # std Soundspeed contrast
              shiny::numericInput("std_h",
                           shiny::actionLink("sdh_info","std(h):"),
                           min = 0,
                           max = 100,
                           value = 0.0015,
                           step=0.0001),
              # Correlation Length
              shiny::numericInput("l_corr",
                           shiny::actionLink("lc_info","Correlation Length [% of L]:"),
                           min = 0,
                           max = 100,
                           value = 20,
                           step=1),
              shiny::actionButton("start_ih","Compute")
            ),
            #simulation
            shiny::tabPanel(
              title = "Simulation",
              shiny::br(),
              shiny::selectInput("output",
                          shiny::actionLink("out_info","Output"),
                          c("Scattering Amplitude",
                            "Cross-section",
                            "TS",
                            "RTS"),
                          "TS",
                          multiple=FALSE),
              shiny::selectInput("variable",
                          shiny::actionLink("var_info","Variable"),
                          c("Frequency (kHz)",
                            "angle (degrees)",
                            "ka"),
                          "Frequency (kHz)",
                          multiple=FALSE),
              # Sample Points
              shiny::numericInput("s_p",
                           shiny::actionLink("sp_info","Sample points:"),
                           min = 0,
                           max = 1000,
                           value = 20,
                           step= 1),
              # Integration points
              shiny::numericInput("int_p",
                           shiny::actionLink("ip_info","Integration Points:"),
                           min = 0,
                           max = 10000,
                           value = 30,
                           step = 1),
              # Variable start value
              shiny::numericInput("v_start",
                           shiny::actionLink("vmin_info","Variable start value:"),
                           min = 1,
                           max = 1000,
                           value = 300,
                           step=1),
              # Variable end value
              shiny::numericInput("v_end",
                           shiny::actionLink("vmax_info","Variable end value:"),
                           min = 0,
                           max = 1000,
                           value = 600,
                           step=1),
              # Frequency
              shiny::numericInput("freq",
                           shiny::actionLink("freq_info","Frequency [kHz]:"),
                           min = 0,
                           max = 1000,
                           value = 120,
                           step=1)

            )
          )),
        # Show a plot of the generated distribution
        shiny::mainPanel(
          shiny::tabsetPanel(id="res",
                      shiny::tabPanel("Results",
                               shiny::br(),
                               shiny::fileInput("load_c","Load Parameter Configuration",accept=c(".dat")),
                               shiny::actionButton("start","Run Model"),
                               shiny::downloadButton("save_c","Save Config"),
                               shiny::downloadButton("export","Export as csv"),
                               shiny::hr(),
                               shiny::plotOutput("shapePlot"),
                               shiny::br(),
                               shiny::plotOutput("resPlot",
                                          hover = shiny::hoverOpts(id = "plot_hover",
                                                            delay = 0),
                                          dblclick = "resPlot_dblclick",
                                          brush = shiny::brushOpts(
                                            id = "resPlot_brush",
                                            resetOnNew = TRUE)),
                               shiny::uiOutput("dynamic"),
                               shiny::br(),
                               shiny::dataTableOutput("resDat")


                      ),
                      shiny::tabPanel(value="ih",
                               title="Inhomogenous Body",
                               shiny::plotOutput("gh"),
                               shiny::br(),
                               shiny::plotOutput("fluc"),
                               shiny::br(),
                               shiny::dataTableOutput("resIh")
                      ),
                      shiny::tabPanel(value="gh_tab",
                               title="Parameter Examples",
                               #shiny::h4("Blabla")
                               #includeMarkdown(paste0(system.file(package="ZooScat"),"/extdata/gh.Rmd"))
                               #includeshiny::HTML(paste0(system.file(package="ZooScat"),"/extdata/gh.shiny::HTML"))
                               shiny::includeHTML(paste0(system.file(package="ZooScat"),"/extdata/gh2.HTML"))
                               #includeshiny::HTML("./extdata/gh.shiny::HTML")
                      ),


                      shiny::tabPanel(value="c_cal",
                               title="Calculate Speed of Sound",
                               shiny::br(),
                               shiny::numericInput("T_in",
                                            "Temperature (degrees Celsius):",
                                            min = 0,
                                            max = 100,
                                            value = 5,
                                            step=.1),
                               shiny::numericInput("S_in",
                                            "Salinity (parts per thousand):",
                                            min = 0,
                                            max = 60,
                                            value = 35,
                                            step=.1),
                               shiny::numericInput("D_in",
                                            "Depth (m):",
                                            min = 0,
                                            max = 10000,
                                            value = 100,
                                            step=1),
                               shiny::numericInput("lat_in",
                                                   "Latitude (°) (Only used for method following (Leroy et al. (2018)):",
                                                   min = -180,
                                                   max = 180,
                                                   value = 45,
                                                   step=.01),
                               shiny::actionLink("c_calc", "Calculate"),
                               shiny::br(),
                               shiny::br(),shiny::br(),
                               shiny::dataTableOutput("speed"),
                               shiny::br(),
                               shiny::hr(),
                               shiny::HTML("<i><strong>Mackenzie (1981):</strong> The empirical equation generally holds validity for a temperature range between 2 and 30 degrees Celsius, Salinities between 25 and 40 parts per thousand and a depth range between 0 and 8000 m</i>"),
                               shiny::br(),shiny::br(),
                               shiny::HTML("<i><strong>Copper (1981):</strong> The empirical equation generally holds validity for a temperature range between 0 and 35 degrees Celsius, Salinities between 0 and 45 parts per thousand and a depth range between 0 and 4000 m</i>"),
                               shiny::br(),shiny::br(),
                               shiny::HTML("<i><strong>Leroy et al (2008):</strong> This empirical equation delivers good agreement with previously formulated formulas, requiring a large number of coefficients (within +/- 0.2 m/s), across all conditions. Results are good for all common ocean conditions except for very high salinities (>>42 % as they can occur in inland seas or hot brine spots at the bottom of some seas)</i>"),
                               shiny::br(),shiny::br(),
                               shiny::h3("References"),
                               shiny::HTML("K.V. Mackenzie, Nine-term equation for the sound speed in the oceans (1981) J.Acoust. Soc. Am. 70(3), pp 807-812"),
                               shiny::br(),shiny::br(),
                               shiny::HTML("A.B. Coppens, Simple equations for the speed of sound in Neptunian waters (1981) J. Acoust. Soc. Am. 69(3), pp 862-863"),
                               shiny::br(),shiny::br(),
                               shiny::HTML("Leroy, C. C., Robinson, S. P., & Goldsmith, M. J. (2008). A new equation for the accurate calculation of sound speed in all oceans. J. Acoust. Soc. Am. 124(5), pp 2774-2782.")
                               ),
                      shiny::tabPanel(value="rhosw_cal",
                                      title="Seawater density",
                                      shiny::br(),
                                      shiny::numericInput("Trho_in",
                                                          "Temperature (degrees Celsius):",
                                                          min = 0,
                                                          max = 100,
                                                          value = 5,
                                                          step=.1),
                                      shiny::numericInput("Srho_in",
                                                          "Salinity (psu +/- parts per thousand):",
                                                          min = 0,
                                                          max = 60,
                                                          value = 35,
                                                          step=.1),
                                      shiny::numericInput("Prho_in",
                                                          "Pressure (Bar):",
                                                          min = 0,
                                                          max = 10000,
                                                          value = 0,
                                                          step=1),
                                      shiny::br(),
                                      shiny::actionLink("rhosw_calc", "Calculate"),
                                      shiny::br(),
                                      shiny::hr(),
                                      shiny::HTML("Density of sea water for a given temperature T in the range 0 < T < 40 ◦C, salinity S in the range 0 < S < 42 PSU and pressure p is determined from the equation of state (UNESCO 1981)"),
                                      shiny::hr(),
                                      shiny::br(),shiny::br(),
                                      shiny::dataTableOutput("density"),
                                      shiny::br(),
                                      shiny::hr(),
                                      shiny::br(),shiny::br(),
                                      shiny::HTML("UNESCO (1981) Tenth report of the joint panel on oceanographic tables and standards. UNESCO Technical Papers in Marine Science, Paris, 25 p.")
                      ),
                      shiny::tabPanel(value="pe_tab",
                                      title="Parameter Explanation",
                                      #shiny::h4("Blabla")
                                      #includeMarkdown(paste0(system.file(package="ZooScat"),"/extdata/gh.Rmd"))
                                      #includeshiny::HTML(paste0(system.file(package="ZooScat"),"/extdata/gh.shiny::HTML"))
                                      shiny::includeHTML(paste0(system.file(package="ZooScat"),"/extdata/parameters.HTML"))
                                      #includeshiny::HTML("./extdata/gh.shiny::HTML")
                      ),
                      shiny::tabPanel("About",
                               shiny::tags$div(shiny::tags$img(src="extdata/ZOOSCAT.png"),style='text-align: center;'),
                               shiny::h4("ZooScat – An R package for modelling the scattering properties of weak scattering targets using the Distorted Wave Born Approximation"),
                               shiny::hr(),
                               shiny::p("Version 0.2"),
                               shiny::hr(),
                               shiny::p("developped by: "),
                               shiny::br(),
                               shiny::HTML("Dr Sven Gastauer<sup>1,2</sup>, "),
                               shiny::HTML("Dr Dezhang Chu<sup>3</sup>, "),
                               shiny::HTML("Dr Martin J. Cox<sup>2</sup>"),
                               shiny::br(),shiny::br(),
                               shiny::p(shiny::HTML("<i>1) Antarctic Climate and Ecosystem Cooperative Research Centre, University of Tasmania, Private Bag 80, Hobart, Tasmania, 7001, sven.gastauer@utas.edu.au</i>")),
                               shiny::p(shiny::HTML("<i>2) Australian Antarctic Division, 203 Channel Highway, Kingston, TAS 7050, Australia</i>")),
                               shiny::p(shiny::HTML("<i>3) Northwest Fisheries Science Center, National Marine Fisheries Service, National Oceanic and Atmospheric Administration, 2725 Montlake Boulevard East, Seattle, Washington 98112, USA</i>")),
                               shiny::p(shiny::tags$div("Based on ZBS-DWBA developed by: Dr Dezhang Chu")),
                               shiny::br(),shiny::br(),
                               shiny::hr(),
                               shiny::em(
                                 shiny::span("Contact"),
                                 shiny::a("Sven Gastauer", href = "mailto:sven.gastauer@utas.edu.au"),
                                 shiny::br(), shiny::br()
                               )


                      )
          ))
      )),


    # Define server logic required to draw a histogram
    server <- function(input, output, session) {

      ####INITIALISATION

      #add path for data files
      shiny::addResourcePath("extdata", system.file("extdata", package="ZooScat"))
      #create a list of reactive values
      values <- shiny::reactiveValues()
      #if para is not available, create an empty list
      if(exists("para")==FALSE){para=list()}

      #show or hide the value examples - initial setup
      if(exists("values$gh_s")==FALSE){
        shiny::hideTab(inputId="res",target="gh_tab")
        values$gh_s=0
        #shiny::hideTab(inputId="res",target="gh_tab")
      }

      #show or hide the sound speed computation tab - initial setup
      if(exists("values$c_t")==FALSE){
        shiny::hideTab(inputId="res",target="c_cal")
        values$c_t=0
      }
      #show or hide the density computation tab - initial setup
      if(exists("values$rhosw_t")==FALSE){
        shiny::hideTab(inputId="res",target="rhosw_cal")
        values$rhosw_t=0
      }
      ####SHOW/HIDE ENABLE/DISABLE TABS AND OPTIONS
      ####MAKE MINOR CALCULATIONS IN SUBTABS

      ###INHOMOGENOUS BODY

      #Conditional inhomogenous menus
      shiny::observeEvent(input$body_ih, {
        if(input$body_ih == TRUE){
          shinyjs::enable("n_seg")
          shinyjs::enable("std_g")
          shinyjs::enable("std_h")
          shinyjs::enable("l_corr")
          shinyjs::enable("start_ih")

          shiny::showTab(inputId="res",target="ih")
          para$shape$body_ih==TRUE
        }
        if(input$body_ih == FALSE){
          shinyjs::disable("n_seg")
          shinyjs::disable("std_g")
          shinyjs::disable("std_h")
          shinyjs::disable("l_corr")
          shinyjs::disable("start_ih")

          shiny::hideTab(inputId="res",target="ih")
          para$shape$body_ih==FALSE
        }

      })

      #Start inhomogenous body
      shiny::observeEvent(input$start_ih,{
        if(exists("para")==FALSE){para <- list()}
        para$phy$seg_no = as.numeric(as.character(input$n_seg))
        para$phy$g_std = as.numeric(as.character(input$std_g))
        para$phy$h_std = as.numeric(as.character(input$std_h))
        para$phy$corrL = as.numeric(as.character(input$l_corr))
        para$phy$g0 = as.numeric(as.character(input$g))
        para$phy$h0 = as.numeric(as.character(input$h))
        para$simu$ni = as.numeric(as.character(input$s_p))
        para$simu$n =  as.numeric(as.character(input$int_p))
        para$phy$body_ih=TRUE

        inh <- inhom_gh(para)

        shiny::updateTabsetPanel(session, "res",selected="ih")
        #gh plot
        output$gh <- shiny::renderPlot({
          print(inh$ghplot)
        })
        #fluctuation plot
        output$fluc <- shiny::renderPlot({
          if(is.null(inh$flucplot) == FALSE){
            print(inh$flucplot)
          }
        })
        values$gh.sum <- as.data.frame(cbind("Variable"=row.names(inh$gh.sum),
                                             "Constructed Values"=round(inh$gh.sum,4)))
        names(values$gh.sum)[2] <- "Constructed values"

        #output$resIh <- shiny::renderDataTable(values$gh.sum)
        output$resIh <- shiny::renderDataTable(values$gh.sum)

        values$g<-as.matrix(inh$g)
        values$h<-as.matrix(inh$h)

      })

      #g and h examples - show or hide
      shiny::observeEvent(input$gbut,{
        if(values$gh_s==0){
          shiny::showTab(inputId="res",target="gh_tab")
          shiny::updateTabsetPanel(session, "res",selected="gh_tab")
          values$gh_s <- values$gh_s+1
        }else{
          shiny::hideTab(inputId="res",target="gh_tab")
          values$gh_s <- 0
        }

      })

      #compute c tab - show or hide
      shiny::observeEvent(input$c_but,{
        if(values$c_t==0){
          shiny::showTab(inputId="res",target="c_cal")
          shiny::updateTabsetPanel(session, "res",selected="c_cal")
          values$c_t <- 1
        }else{
          shiny::hideTab(inputId="res",target="c_cal")
          values$c_t <- 0
        }

      })

      #compute c - action
      shiny::observeEvent(input$c_calc,{
        cm <- round(c_Mackenzie1981(D=input$D_in,S=input$S_in,T=input$T_in),3)
        cc <- round(c_Coppens1981(D=input$D_in,S=input$S_in,T=input$T_in),3)
        cl <- round(c_Leroy08(Z=input$D_in,S=input$S_in,T=input$T_in, lat=input$lat_in),3)

        sp <- as.data.frame(cbind(cm,cc,cl))
        names(sp)<- c("MacKenzie 1981","Copper 1981","Leroy 2008")
        sp$unit <- "m/s"
        output$speed <- shiny::renderDataTable(sp)

      })

      #compute rhosw tab - show or hide
      shiny::observeEvent(input$rhosw_but,{
        if(values$rhosw_t==0){
          shiny::showTab(inputId="res",target="rhosw_cal")
          shiny::updateTabsetPanel(session, "res",selected="rhosw_cal")
          values$rhosw_t <- 1
        }else{
          shiny::hideTab(inputId="res",target="rhosw_cal")
          values$rhosw_t <- 0
        }

      })

      #compute rhosw - action
      shiny::observeEvent(input$rhosw_calc,{

        rsw <- round(rho(p=input$Prho_in,S=input$Srho_in,T=input$Trho_in),3)

        sp <- as.data.frame(rsw)
        names(sp)<- c("Density")
        output$density <- shiny::renderDataTable(sp)

      })

      ###SHAPE

      #Length average - enable/disable options
      shiny::observeEvent(input$l_avg, {
        if(input$l_avg == FALSE){
          shinyjs::disable("stdll")
          shinyjs::disable("l_incr")
        }
        if(input$l_avg == TRUE){
          shinyjs::enable("stdll")
          shinyjs::enable("l_incr")
        }
      })

      #profile - enable/disable options
      shiny::observeEvent(input$profile_cb, {
        shiny::req(para)

        if(input$profile_cb == FALSE){
          shinyjs::disable("prof_fn")
          shinyjs::disable("tap_sm")
          shinyjs::disable("ax_sm")
          if(exists("para")){ para$shape$prof_name == -1}
        }else{
          shinyjs::enable("prof_fn")
          shinyjs::enable("tap_sm")
          shinyjs::enable("ax_sm")
          #para$shape$prof_name == 1
        }
      })

      ###ORIENTATION

      #Orientation average - enable/disable options
      shiny::observeEvent(input$theta_flag, {
        if(input$theta_flag == FALSE){
          shinyjs::disable("theta_distr")
          shinyjs::disable("theta_sd")
          shinyjs::disable("theta_inc")
          shinyjs::disable("theta_min")
          shinyjs::disable("theta_max")
        }else{
          shinyjs::enable("theta_distr")
          if(input$theta_distr==c("Uniform PDF","Gaussian PDF")[1]){
            shinyjs::enable("theta_min")
            shinyjs::enable("theta_max")
            shinyjs::disable("theta_sd")
            shinyjs::disable("theta_inc")
          }
          if(input$theta_distr==c("Uniform PDF","Gaussian PDF")[2]){
            shinyjs::enable("theta_sd")
            shinyjs::enable("theta_inc")
            shinyjs::disable("theta_min")
            shinyjs::disable("theta_max")

          }
        }
      })
      #Orientation distribution - enable/disable
      shiny::observeEvent(input$theta_distr,{
        shiny::req(input$theta_distr)
        if(input$theta_distr==c("Uniform PDF","Gaussian PDF")[1]){
          shinyjs::enable("theta_min")
          shinyjs::enable("theta_max")
          shinyjs::disable("theta_sd")
          shinyjs::disable("theta_inc")
        }
        if(input$theta_distr==c("Uniform PDF","Gaussian PDF")[2]){
          shinyjs::enable("theta_sd")
          shinyjs::enable("theta_inc")
          shinyjs::disable("theta_min")
          shinyjs::disable("theta_max")

        }
      })

      ####MAIN ACTION

      ###Load config
      shiny::observeEvent(input$load_c,{

        #Set filename to config file
        values$fname = input$load_c$datapath

        #read config file
        para = read_para(values$fname)

        ### Set the para values
        shiny::updateNumericInput(session, "l_inp", value=para$shape$L)
        shiny::updateNumericInput(session, "rho_inp", value=para$shape$rho_L)
        shiny::updateNumericInput(session, "la_inp", value=para$shape$L_a)
        shiny::updateNumericInput(session, "taper_inp", value=para$shape$order)
        shiny::updateCheckboxInput(session, "l_avg", value=ifelse(para$shape$ave_flag==1,TRUE,FALSE))
        shiny::updateNumericInput(session, "stdll", value=para$shape$Lstd)
        shiny::updateNumericInput(session, "l_incr", value=para$shape$dL)
        shiny::updateNumericInput(session, "tap_sm", value=para$shape$taper_sm)
        shiny::updateNumericInput(session, "ax_sm", value=para$shape$axis_sm)
        if(is.null(para$shape$prof_name)){
          para$shape$prof_name==-1
          shiny::updateCheckboxInput(session, "profile_cb", value=FALSE)
        }
        shiny::updateCheckboxInput(session, "profile_cb", value=ifelse(para$shape$prof_name==-1,FALSE,TRUE))

        values$prof_fn <- para$shape$prof_name

        shiny::updateNumericInput(session, "theta_avg", value=para$orient$angm)
        shiny::updateNumericInput(session, "theta_min", value=para$orient$ang0)
        shiny::updateNumericInput(session, "theta_max", value=para$orient$ang1)
        shiny::updateCheckboxInput(session,  "theta_flag", value=ifelse(para$shape$prof_name==1,TRUE,FALSE))
        shiny::updateRadioButtons(session, "theta_distr", selected=c("Uniform PDF","Gaussian PDF")[para$orient$PDF])
        shiny::updateNumericInput(session, "theta_sd", value=para$orient$PDF_para)
        shiny::updateNumericInput(session, "theta_inc", value=para$orient$dang)

        shiny::updateNumericInput(session, "g", value=para$phy$g0)
        shiny::updateNumericInput(session, "h", value=para$phy$h0)
        shiny::updateCheckboxInput(session, "body_ih", value=ifelse(para$shape$prof_name > 1,TRUE,FALSE))
        shiny::updateNumericInput(session, "n_seg", value=para$phy$seg_no)
        shiny::updateNumericInput(session, "std_g", value=para$phy$g_std)
        shiny::updateNumericInput(session, "std_h", value=para$phy$h_std)
        shiny::updateNumericInput(session, "l_corr", value=para$phy$corrL)


        shiny::updateNumericInput(session, "int_p", value=para$simu$n)
        shiny::updateNumericInput(session, "s_p", value=para$simu$ni)
        shiny::updateSelectInput(session, "output", selected=c("Scattering Amplitude", "Cross-section","TS","RTS")[para$simu$out_indx])
        shiny::updateSelectInput(session, "variable", selected=c("Frequency (kHz)", "angle (degrees)","ka")[para$simu$var_indx])
        shiny::updateNumericInput(session, "v_start", value=para$simu$var0)
        shiny::updateNumericInput(session, "v_end", value=para$simu$var1)
        shiny::updateNumericInput(session, "freq", value=para$simu$freq)

      })

      ### START THE MODEL

      #Start button press
      shiny::observeEvent(input$start,{
        # generate bins based on input$bins from ui.R
        shiny::withProgress(message = 'Loading...', value=0,{

          #Get the parameters
          bins <- seq(input$theta_min, input$theta_max, length.out = 100)
          para <- list()
          #get shape values
          para$shape=list()
          para$shape$L = as.numeric(as.character(input$l_inp))
          para$shape$rho_L = as.numeric(as.character(input$rho_inp))
          para$shape$L_a = as.numeric(as.character(input$la_inp))
          para$shape$order = as.numeric(as.character(input$taper_inp))
          para$shape$ave_flag = ifelse(input$l_avg==TRUE,1,0)
          para$shape$Lstd = as.numeric(as.character(input$stdll))
          para$shape$dL = as.numeric(as.character(input$l_incr))

          if(input$profile_cb ==FALSE){
            para$shape$prof_name = -1
          }else{
            ifelse(!is.null(input$prof_fn$datapath),
                   para$shape$prof_name <- input$prof_fn$datapath,
                   para$shape$prof_name <- values$prof_fn)

          }
          values$f <- input$prof_fn$name

          para$shape$axis_sm = as.numeric(as.character(input$ax_sm))
          para$shape$taper_sm = as.numeric(as.character(input$tap_sm))

          #get orientation parameters
          para$orient = list()
          para$orient$angm = as.numeric(as.character(input$theta_avg))
          para$orient$ang0 = as.numeric(as.character(input$theta_min))
          para$orient$ang1 = as.numeric(as.character(input$theta_max))
          para$orient$ave_flag = ifelse(input$theta_flag==TRUE,1,0)
          para$orient$theta_distr = as.character(input$theta_distr)
          para$orient$PDF <- as.numeric(as.character(ifelse(para$orient$theta_distr=="Uniform PDF",1,2)))
          para$orient$PDF_para = as.numeric(as.character(input$theta_sd))
          para$orient$dang = as.numeric(as.character(input$theta_inc))


          # get physical parameters
          para$phy <- list()
          para$phy$g0 = as.numeric(as.character(input$g))
          para$phy$h0 = as.numeric(as.character(input$h))

          if(is.null(para$phy$body_ih)){para$phy$body_ih=FALSE}

          if(input$body_ih==TRUE){
            para$phy$body_ih = TRUE
            para$phy$g <- values$g
            para$phy$h <- values$h
            #print(para$phy$body_ih)
          }else{
            para$phy$body_ih <- FALSE
          }


          para$phy$seg_no = as.numeric(as.character(input$n_seg))
          para$phy$g_std = as.numeric(as.character(input$std_g))
          para$phy$h_std = as.numeric(as.character(input$std_h))
          para$phy$corrL = as.numeric(as.character(input$l_corr))

          # get simulation parameters
          para$simu <- list()
          outp <- input$output
          out_opts <- c("Scattering Amplitude", "Cross-section","TS","RTS")
          para$simu$out_indx = which(out_opts==outp)
          varp <- input$variable
          var_opts <- c("Frequency (kHz)", "angle (degrees)","ka")
          para$simu$var_indx = which(var_opts==varp)
          para$simu$ni = as.numeric(as.character(input$s_p))
          para$simu$n = as.numeric(as.character(input$int_p))
          para$simu$var0 = as.numeric(as.character(input$v_start))
          para$simu$var1 = as.numeric(as.character(input$v_end))
          para$simu$freq = as.numeric(as.character(input$freq))


          #Create list with soundspeed info
          misc <- list()
          misc$cw <- input$c

          ###RUN THE MODEL
          #Run DWBA based on config file
          #print(para)
          options(warn=-1)
          res = bscat(para=para, misc=misc, app=TRUE)
          values$para <- para

          options(warn=0)
          #plot shape
          output$shapePlot <- shiny::renderPlot({
            print(res$shplot)
          })

          #plot results
          ranges <- shiny::reactiveValues(x = NULL, y = NULL)

          shiny::setProgress(value=0.9,message="Generating plots...")

          #Model output plot
          output$resPlot <- shiny::renderPlot({
            indat <- as.data.frame(cbind(x=res$var, 'y'=res$y))
            names(indat) <- c('x','y')
            ggplot2::ggplot(data=indat)+
              ggplot2::geom_line(
                ggplot2::aes(x=x,y=y),lwd=1.5)+
              ggplot2::xlab(res$xlab)+
              ggplot2::ylab(res$ylab)+
              ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = c(0.2,0.2))+
              ggplot2::theme_bw() +
              ggplot2::theme(panel.border =
                               ggplot2::element_blank(),
                    panel.grid.major =
                      ggplot2::element_blank(),
                    panel.grid.minor =
                      ggplot2::element_blank(),
                    axis.line =
                      ggplot2::element_line(colour = "black"),
                    axis.text =
                      ggplot2::element_text(size=18),
                    axis.title =
                      ggplot2::element_text(size=18))
          })

          #Zoom settings
          shiny::observeEvent(input$resPlot_dblclick, {
            brush <- input$resPlot_brush
            if (!is.null(brush)) {
              ranges$x <- c(brush$xmin, brush$xmax)
              ranges$y <- c(brush$ymin, brush$ymax)

            } else {
              ranges$x <- NULL
              ranges$y <- NULL
            }
          })

          # Get plot values
          output$dynamic <- shiny::renderUI({
            shiny::req(input$plot_hover)
            shiny::verbatimTextOutput("vals")
          })
          # Show cursor position values
          output$vals <- shiny::renderPrint({
            hover <- input$plot_hover
            # print(str(hover)) # list
            indat <- as.data.frame(cbind(x=res$var, y=res$y))
            names(indat) <- c("x","y")
            y <- shiny::nearPoints(indat,
                            input$plot_hover,
                            threshold = 1000,
                            maxpoints = 1,
                            addDist = TRUE)
            y
          })

          #create results data table
          shiny::setProgress(value=0.95,message="Generating data table...")

          values$results <- as.data.frame(cbind(Var = res$var, Val = res$y))
          names(values$results) <- c(input$variable,input$output)

          ron <- switch(input$output,
                        'Scattering Amplitude' = 3,
                        'Cross-section'=5,
                        'TS'=2,
                        'RTS'=2)

          output$resDat <- shiny::renderDataTable(round(values$results,ron))
          shiny::setProgress(value=0.1,message="Done...")

        })
      })

      # Export data table
      output$export <- downloadHandler(
        filename = function(){
          paste("DWBA", ".csv",sep="")
        },
        content = function(file) {
          write.csv(values$results, file, row.names=FALSE)
        }
      )

      #Export config
      output$save_c <- downloadHandler(
        filename = function(){
          paste("config", ".dat",sep="")
        },
        content = function(file){
          values$para$prof_fn <- values$f
          createParaDat(para=values$para,fn=file)
        }
      )


      #############################################
      ######MESSAGES

      ####inhomogenous body tab
      #Error if number of segments is 0
      shiny::observeEvent(input$n_seg,{
        shiny::req(input$n_seg)
        if(input$n_seg<1){
          shiny::showNotification("ERROR: The number of segments must be bigger or equal to 1", type="error")
        }
      })
      #warning if g is too high
      shiny::observeEvent(input$g,{
        shiny::req(input$g)
        if(input$g > 1.05 | input$g < 0.95){
          shiny::showNotification("WARNING: The provided density contrast (g) is out of the recommended value range. The DWBA model works best for low scattering targets,
                           with a g closer to 1 (within +/- 5%).",type="warning", closeButton=TRUE, duration=10)
        }
        })
      #warning if h is too high
      shiny::observeEvent(input$h,{
        shiny::req(input$h)
        if(input$h > 1.05 | input$h < 0.95){
          shiny::showNotification("WARNING: The provided sound speed contrast (h) is out of the recommended value range. The DWBA model works best for low scattering targets,
                           with a h close to 1 (within +/- 5%).",type="warning", closeButton=TRUE, duration=10)
        }
        })
      #variable = angle
      shiny::observeEvent(input$variable, {
        if(input$variable == "angle (degrees)"){
          shinyjs::enable("freq")
        }else{
          shinyjs::disable("freq")
        }
      })

      #### INFO MESSAGES
      #length
      shiny::observeEvent(input$l_info,{
        shiny::showNotification(shiny::HTML("The body length in mm"),
                         type="message",
                         duration = 10)})
      #rho/L
      shiny::observeEvent(input$rhoL_info,{
        shiny::showNotification(shiny::HTML("&rho;<sub>c</sub> is the radius of curvature (assuming a uniformly bent cylinder).
                              <br>L is the body length (mm) of the elongated target.</br>
                              <br>If the ratio &rho;<sub>c</sub>/L approaches infinity, a straight cylinder will be the result.</br>"),
                         type="message",
                         duration = 10)

      })

      #L/a
      shiny::observeEvent(input$la_info,{
        shiny::showNotification(shiny::HTML("a is the radius of the mid-point of the cylinder.
                              <br>L is the length of the elongated body (mm).</br>
                              L/a is the ratio of the two input parameters"),
                         type="message",
                         duration = 10)

      })
      #average L
      shiny::observeEvent(input$lav_info,{
        shiny::showNotification(shiny::HTML("If the length average checkbox is ticked, the model average over length (L, in mm) will be performed.
                              <br> std(L)/<span style='text-decoration:overline;'>L</span> - the ratio of the standard deviation of length (std(L) to the mean length (<span style='text-decoration:overline;'>L</span>)</br>"),
                         type="message",
                         duration = 10)
      })

      shiny::observeEvent(input$lav_info2,{
        shiny::showNotification(shiny::HTML("If the length average checkbox is ticked, the model average over length (L, in mm) will be performed.
                              <br> std(L)/<span style='text-decoration:overline;'>L</span> - the ratio of the standard deviation of length (std(L) to the mean length (<span style='text-decoration:overline;'>L</span>)</br>"),
                         type="message",
                         duration = 10)
      })
      #L increment
      shiny::observeEvent(input$linc_info,{
        shiny::showNotification(shiny::HTML("Defines the increment of the length, if an average length is calculated. This value has to be chosen with care, as it has to be possible to calculate the provided std(L)/mean(L) around the provided mean L with the given increment."),
                         type="message",
                         duration = 10)

      })

      #Profile
      shiny::observeEvent(input$prof_info,{
        shiny::showNotification(shiny::HTML("If this option is checked, a profile file containing 3 or 5 columns can be loaded.
                              <br>The profile file must contain at least 3 columns, containingX,Z,R coordinates, with:</br>
                              <br>X,Z = center coordinates of each circular segment</br>
                              <br>R = radius of each circular segment</br>
                              <br>The additional, optional parameters are X<sub>upper</sub> and X<sub>lower</sub> which are X +/- R. </br>"),
                         type="message",
                         duration = 10)

      })
      #taper order
      shiny::observeEvent(input$taper_info,{
        shiny::showNotification(shiny::HTML("The taper order (n) is a parameter controlling the tapering (r(z)): r(z)= <span style='white-space: nowrap; font-size:larger'>
                              &radic;<span style='text-decoration:overline;'>&nbsp;1 - (2z/L)<sup>n</sup>&nbsp;</span>
                              </span>, with z=[-L/s;L/2]. For a straight  cylinder (&rho;<sub>c</sub>/L -> infinity), n=2 results in a prolate spheroid if L>2a, or in an oblate spheroid for L<2a"),
                         type="message",
                         duration = 10)
      })

      #taper smooth
      shiny::observeEvent(input$tapsm_info,{
        shiny::showNotification(shiny::HTML("Filter length to smooth the radius (R), where 1 = no smoothing"),
                         type="message",
                         duration = 10)
      })
      #axis smooth
      shiny::observeEvent(input$axsm_info,{
        shiny::showNotification(shiny::HTML("Filter length to smooth the body axis (X, Z), where 1 = no smoothing"),
                         type="message",
                         duration = 10)
      })
      #mean theta
      shiny::observeEvent(input$mtheta_info,{
        shiny::showNotification(shiny::HTML("Mean angle of orientation (incident angle)<span style='text-decoration:overline;'>&theta;</span>, in degrees, if no average is computed this will be the only value considered"),
                         type="message",
                         duration = 10)
      })
      #Begin theta
      shiny::observeEvent(input$mintheta_info,{
        shiny::showNotification(shiny::HTML("Minimum angle of orientation (&theta;<sub>min</sub>) for uniform probability density function (PDF)"),
                         type="message",
                         duration = 10)
      })
      #Stop theta
      shiny::observeEvent(input$maxtheta_info,{
        shiny::showNotification(shiny::HTML("Maximum angle of orientation (&theta;<sub>max</sub>) for uniform probability density function (PDF)"),
                         type="message",
                         duration = 10)
      })
      #Average theta
      shiny::observeEvent(input$avtheta_info,{
        shiny::showNotification(shiny::HTML("<br>An average over a range of orientation angles (&theta;) will be performed if this box is checked. Two different probability distribution functions (PDF) can be chosen for the orientation angle distribution:</br>
                              <br><strong>Uniform PDF:</strong> the &theta; distribution will follow a uniform PDF with the range defined by Min. Theta (&theta;<sub>min</sub>) and Max. Theta (&theta;<sub>max</sub>) </br>
                              <br><strong>Gaussian PDF:</strong> the &theta; distribution will follow a Gaussian PDFaround the mean &theta; following two parameter:</br>
                              <ul style='list-style-type:disc'>
                              <li>std(&theta;): the standard deviation of &theta; with the computation range [<span style='text-decoration:overline;'>&theta;</span> - 3 * std(&theta;); <span style='text-decoration:overline;'>&theta;</span> + 3 * std(&theta;)]</li>
                              <li>&theta; increment: angular increment when performing the average computation</li>
                              </ul>"),
                         type="message",
                         duration = 10)
      })
      #Distribution
      #Average theta
      shiny::observeEvent(input$tdist_info,{
        shiny::showNotification(shiny::HTML("<br>An average over a range of orientation angles (&theta;) will be performed if the Orientation Average checkbox is ticked. Two different probability distribution functions (PDF) can be chosen for the orientation angle distribution:</br>
                              <br><strong>Uniform PDF:</strong> the &theta; distribution will follow a uniform PDF with the range defined by Min. Theta (&theta;<sub>min</sub>) and Max. Theta (&theta;<sub>max</sub>) </br>
                              <br><strong>Gaussian PDF:</strong> the &theta; distribution will follow a Gaussian PDFaround the mean &theta; following two parameter:</br>
                              <ul style='list-style-type:disc'>
                              <li>std(&theta;): the standard deviation of &theta; with the computation range [<span style='text-decoration:overline;'>&theta;</span> - 3 * std(&theta;); <span style='text-decoration:overline;'>&theta;</span> + 3 * std(&theta;)]</li>
                              <li>&theta; increment: angular increment when performing the average computation</li>
                              </ul>"),
                         type="message",
                         duration = 10)
      })

      #std theta
      shiny::observeEvent(input$sdtheta_info,{
        shiny::showNotification(shiny::HTML("This will only have an effect if an Orientation average with a Gaussian PDF is to be calculated.
                              <br>std(&theta;) is one of the two parameter needed to compute the Gaussian PDF of &theta;, defined standard deviation of &theta; with the computation range [<span style='text-decoration:overline;'>&theta;</span> - 3 * std(&theta;); <span style='text-decoration:overline;'>&theta;</span> + 3 * std(&theta;)]</br>
                              <br>&theta; increment: is the second parameter, which is the angular increment when performing the average computation with a Gaussian PDF</br>"),
                         type="message",
                         duration = 10)
      })
      #theta increment
      shiny::observeEvent(input$inctheta_info,{
        shiny::showNotification(shiny::HTML("This will only have an effect if an Orientation average with a Gaussian PDF is to be calculated.
                              <br>std(&theta;) is one of the two parameter needed to compute the Gaussian PDF of &theta;, defined standard deviation of &theta; with the computation range [<span style='text-decoration:overline;'>&theta;</span> - 3 * std(&theta;); <span style='text-decoration:overline;'>&theta;</span> + 3 * std(&theta;)]</br>
                              <br>&theta; increment: is the second parameter, which is the angular increment when performing the average computation with a Gaussian PDF</br>"),
                         type="message",
                         duration = 10)
      })

      ##Material prperties
      #g
      shiny::observeEvent(input$g_info,{
        shiny::showNotification(shiny::HTML("For a target with a homogenous density structure, the density contrast (g) is defined as the ratio of the density of the animal to the density in the surrounding fluid."),
                         type="message",
                         duration = 10)
      })
      #h
      shiny::observeEvent(input$h_info,{
        shiny::showNotification(shiny::HTML("For a target with a homogenous sound speed structure, the sound speed contrast (h) is defined as the ratio of the sound speed in the animal to the sound speed in the surrounding fluid."),
                         type="message",
                         duration = 10)
      })
      #c
      shiny::observeEvent(input$c_info,{
        shiny::showNotification(shiny::HTML("Sound speed in m/s in the surrounding fluid"),
                         type="message",
                         duration = 10)
      })
      #ih
      shiny::observeEvent(input$ih_info,{
        shiny::showNotification(shiny::HTML("<br>The inhomogenous body checkbox should be checked if the target should have an inhomogenous body.</br>
                              <ul style='list-style-type:disc'>
                              <li>Mean density contrast and mean sound speed contrast will be used, as defined above.</li>
                              <li>N body Segments: Defines the number of segments with different g<sub>i</sub> and h<sub>i</sub> along the body axis</li>
                              <li>std(g): Standard deviation of the density contrast (g)</li>
                              <li>std(h): Standard deviation of the sound speed contrast (h)</li>
                              <li>Correlation Length (% of L): This parameter will only be considered if the number of body segments is > 8. This parameter controls the variability of g and h along the body axis. A larger variability is set around the mid-section with a decreasing variability to the body extremes. The correlation length is defined as the standard deviaiton of a Gaussian function centered at the mid-section of the body.</li>"),
                         type="message",
                         duration = 10)
      })
      #nseg
      shiny::observeEvent(input$nseg_info,{
        shiny::showNotification(shiny::HTML("N body Segments: Defines the number of segments with different g<sub>i</sub> and h<sub>i</sub> along the body axis, if an inhomogenous body should be computed.
                              <br>If the number of body segments is 8 or less, an uncorrelated computation will be completed. If the number of segments is >8 length correlation will be taken into account. Length correlation controls the variability of g and h along the body axis. A larger variability is set around the mid-section with a decreasing variability to the body extremes. The correlation length is defined as the standard deviation of a Gaussian function centered at the mid-section of the body.</br>"),
                         type="message",
                         duration = 10)
      })
      #sdg
      shiny::observeEvent(input$sdg_info,{
        shiny::showNotification(shiny::HTML("std(g): Standard deviation of the density contrast (g), if an inhomogenous body should be computed"),
                         type="message",
                         duration = 10)
      })
      #sdh
      shiny::observeEvent(input$sdh_info,{
        shiny::showNotification(shiny::HTML("std(h): Standard deviation of the sound speed contrast (h), if an inhomogenous body should be computed"),
                         type="message",
                         duration = 10)
      })
      #lc
      shiny::observeEvent(input$lc_info,{
        shiny::showNotification(shiny::HTML("Correlation Length (% of L): This parameter will only be considered if the number of body segments is > 8. This parameter controls the variability of g and h along the body axis. A larger variability is set around the mid-section with a decreasing variability to the body extremes. The correlation length is defined as the standard deviaiton of a Gaussian function centered at the mid-section of the body, if an inhomogenous body should be computed"),
                         type="message",
                         duration = 10)
      })

      ###Simulation
      #sp
      shiny::observeEvent(input$sp_info,{
        shiny::showNotification(shiny::HTML("Sample points: Defines the number of output variable and computed quantity points"),
                         type="message",
                         duration = 10)
      })
      #ip
      shiny::observeEvent(input$ip_info,{
        shiny::showNotification(shiny::HTML("Integration points: Defines the number of integration points along the body axis"),
                         type="message",
                         duration = 10)
      })
      #vmin
      shiny::observeEvent(input$vmin_info,{
        shiny::showNotification(shiny::HTML("Variable Start Value and Variable End Value define the start and end values of the variable chosen from the Variable drop-down menu"),
                         type="message",
                         duration = 10)
      })

      #vmmax
      shiny::observeEvent(input$vmax_info,{
        shiny::showNotification(shiny::HTML("Variable Start Value and Variable End Value define the start and end values of the variable chosen from the Variable drop-down menu"),
                         type="message",
                         duration = 10)
      })
      #freq
      shiny::observeEvent(input$freq_info,{
        shiny::showNotification(shiny::HTML("Frequency: This variable will only have an effect if the chosen variable is Angle (deg). Frequency than becomes the only frequency used, for which the chosen ouput will be computed for a range of orientations."),
                         type="message",
                         duration = 10)
      })
      #out
      shiny::observeEvent(input$out_info,{
        shiny::showNotification(shiny::HTML("Output is the output quantity of the model. 4 options are available:
                              <ul style='list-style-type:disc'>
                              <li>backscattering amplitude</li>
                              <li>differential backscattering cross-section</li>
                              <li>Target strength (dB re m<sup>2<sup>)</li>
                              <li>Reduced Target Strength (dB re m<sup>2</sup>) (Assuming a TS equation under the shape of TS=20*log(L) + b</li>"),
                         type="message",
                         duration = 10)
      })
      #var
      shiny::observeEvent(input$var_info,{
        shiny::showNotification(shiny::HTML("Variable is the variable of the model. 3 options are available:
                              <ul style='list-style-type:disc'>
                              <li>frequency (kHz)</li>
                              <li>Angle (degrees)</li>
                              <li>ka, with the wave number k and a the radius of the mid-point of the cylinder, determined from the two parameter Length (L) and the ratio L/a</li>"),
                         type="message",
                         duration = 10)
      })

      }
    )
  }
