#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cyborg"),
   # Application title
   titlePanel("Inhomogenous body"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        tags$div(title="Number of segments",
                 numericInput("n_seg",
                              "N of segments:",
                              min = 0,
                              max = 100000,
                              value = 7, step=1)),
        tags$div(title="g - density contrast",
                 numericInput("g",
                              "g:",
                              min = 0,
                              max = 2,
                              value = 1.0357, step=0.0001)),
        tags$div(title="h - sound speed contrast",
                 numericInput("h",
                              "h:",
                              min = 0,
                              max = 2,
                              value = 1.0279, step=0.0001)),
        tags$div(title="Standard Deviation of g",
                 numericInput("g_std",
                              "sd(g):",
                              min = 0.0001,
                              max = 2,
                              value = 0.0016, step=0.0001)),
        tags$div(title="Standard Deviation of h",
                 numericInput("h_std",
                              "sd(h):",
                              min = 0.0001,
                              max = 2,
                              value = 0.0015, step=0.0001)),
        tags$div(title="Correlation Length - % of body length",
                 numericInput("cor_L",
                              "Correlation Length:",
                              min = 0,
                              max = 100,
                              value = 20, step=0.01))
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("corgh")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate g &h

      # plot the new g and h

   })
}

# Run the application
shinyApp(ui = ui, server = server)

