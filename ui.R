library(shinyBS)

shinyUI(fluidPage(
  
  # suppresses spurious 
  # 'progress' error messages after all the debugging 
  # is done:
  #tags$style(type="text/css",
  #           ".shiny-output-error { visibility: hidden; }",
  #           ".shiny-output-error:before { visibility: hidden; }"
  #),
  HTML("<hr>"),
  # the main stuff:
    # Application title
    #titlePanel(""),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        width=4,
        checkboxInput("uniform", label = "Google version", value = FALSE),
        sliderInput("mean1",
                    label = "Separation of appraisal means", 
                    min = 0.0, max = 2.0, step=0.01,
                    value = 0.05),
        sliderInput("upper1", 
                    label = "Uniform appraisal range (Google version)", 
                    min = 100, max = 200, step=1,
                    value = 101),
        sliderInput("lr",
                    label = "Rate of churn", 
                    min = 0.0, max = 1.0, step = 0.05,
                    value = 0.15),
        sliderInput("n.steps", 
                    label = "Nr simulation time steps:", 
                    min = 0, max = 100, step = 1,
                    value = 20),
        sliderInput("n.levels", 
                     label = "Nr job grades:",
                     min = 1, max = 10, step = 1,
                     value = 6),
        wellPanel(actionButton("simulate", "Run simulation"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        width=8,
        plotOutput("profile_plot"),
        plotOutput("appraisal_plot")
      )
    )
  )
)