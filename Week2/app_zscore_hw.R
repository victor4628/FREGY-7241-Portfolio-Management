##############################
# This is a shiny app with a histogram of the price z-scores.
# To run it, just press the "Run App" button on upper right of this panel.
##############################

##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

library(rutils)

# Load the SPY prices
symboln <- "SPY"
pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))

# End setup code
##############################


##############################
## Define the user interface

## Create elements of the user interface
uifun <- shiny::fluidPage(
  
  titlePanel("Histogram of Price Z-scores"),
  
  # Create single row with inputs
  fluidRow(
    
    # Create number of bins for the histogram
    column(width=3, 
           sliderInput(inputId="nbins", 
                       label="Number of bins:", 
                       min=10, max=100, value=50, step=5)
    ),  # end column
    
    # Input lambda decay parameter
    column(width=3, 
           sliderInput(inputId="lambdaf", 
                       label="Lambda decay:", 
                       min=0.01, max=0.99, value=0.9, step=0.01)
    ),  # end column
    
    # Render the plot
    column(width=6,
           plotOutput("plotobj")
    )  # end column
    
  ),  # end fluidRow
  
)  # end user interface



##############################
## Define the server function, with the arguments "input" and "output".
# The server function performs the calculations and creates the plots.

servfun <- function(input, output) {
  
  # Calculate the z-scores
  zscores <- shiny::reactive({
    cat("Calculating the z-scores\n")
    
    # Get model parameters from input argument
    lambdaf <- input$lambdaf
    
    # Calculate the z-scores
    # Use the function HighFreq::run_var() to calculate the
    # EMA variance.
    volp <- HighFreq::run_var(pricev, lambdaf=lambdaf)
    pricema <- volp[, 1]  # EMA price
    volp <- sqrt(volp[, 2])  # EMA volatility
    
    # Calculate z-scores
    (pricev - pricema)/volp
    
  })  # end z-scores
  
  
  # Plot the data
  output$plotobj <- shiny::renderPlot({
    cat("Plotting the z-scores\n")
    
    # Get z-scores and number of bins
    zscoresv <- zscores()
    nbins <- input$nbins
    
    # Create histogram
    hist(zscoresv, breaks=nbins, 
         main="Distribution of Price Z-scores",
         xlab="Z-scores", 
         ylab="Frequency",
         col="lightblue",
         border="white")
    
  })  # end renderPlot
  
}  # end servfun


## Return a Shiny app object

shiny::shinyApp(ui=uifun, server=servfun)
