##############################
# Shiny app for the dual crossover EMA volatility strategy.
# Strategy logic:
#   - Calculate fast and slow EMA volatilities using HighFreq::run_var()
#   - posv = trend * ifelse(fast vol < slow vol, 1, -1)
#   - trend=1  : long when fast < slow (momentum)
#   - trend=-1 : short when fast < slow (mean-reversion)
# To run: press the "Run App" button in RStudio.
##############################

##############################
# Setup code - runs once at startup

library(shiny)
library(rutils)
library(dygraphs)

# Bid-ask spread
bidask <- 0.0001

# End setup code
##############################


##############################
## Define the user interface

uifun <- shiny::fluidPage(

  titlePanel("Dual Crossover EMA Volatility Strategy"),

  # Single controls row
  fluidRow(
    column(width=2,
      selectInput("symbol", label="Symbol",
        choices=rutils::etfenv$symbolv, selected="SPY")
    ),
    column(width=3,
      sliderInput("lambdaf", label="Fast lambda:",
        min=0.1, max=0.99, value=0.76, step=0.01)
    ),
    column(width=3,
      sliderInput("lambdas", label="Slow lambda:",
        min=0.1, max=0.99, value=0.92, step=0.01)
    ),
    column(width=2,
      selectInput("trend", label=HTML("Trend (1)<br>Revert (-1)"),
        choices=c("1", "-1"), selected="-1")
    ),
    column(width=2,
      selectInput("shade", label="Add shading?",
        choices=c("False", "True"), selected="False")
    )
  ),  # end fluidRow

  # Dygraph output
  dygraphs::dygraphOutput("plotobj", height="500px")

)  # end uifun


##############################
## Define the server function

servfun <- function(input, output) {

  # Reactive: load data and compute strategy
  stratdata <- shiny::reactive({
    cat("Computing strategy for", input$symbol, "\n")

    symboln <- input$symbol
    lambdaf <- as.numeric(input$lambdaf)
    lambdas <- as.numeric(input$lambdas)
    trend   <- as.numeric(input$trend)

    # Load OHLC and compute log returns
    ohlc <- get(symboln, rutils::etfenv)
    retp <- rutils::diffit(log(quantmod::Cl(ohlc)))
    colnames(retp) <- symboln

    # Fast and slow EMA volatilities
    volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
    vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])

    # Positions: long (+1) when fast < slow, short (-1) otherwise
    # Multiplied by trend (+1 = follow, -1 = reverse)
    posv <- trend * ifelse(volf < vols, 1, -1)
    posv <- rutils::lagit(posv)

    # Number of position changes (trades)
    ntrades <- sum(abs(rutils::diffit(posv)) > 0)

    # PnL minus transaction costs
    pnls  <- retp * posv
    costv <- 0.5 * bidask * abs(rutils::diffit(posv))
    pnls  <- pnls - costv
    # Scale PnL volatility to that of the ETF
    pnls  <- pnls * sd(retp[retp < 0]) / sd(pnls[pnls < 0])
    colnames(pnls) <- "Strategy"

    # Annualised Sharpe ratios (zero risk-free rate)
    sharpei <- sqrt(252) * mean(retp) / sd(retp)
    sharpes <- sqrt(252) * mean(pnls)  / sd(pnls)

    list(retp=retp, pnls=pnls, posv=posv,
         symboln=symboln, ntrades=ntrades,
         sharpei=round(sharpei, 3),
         sharpes=round(sharpes, 3))
  })  # end reactive


  # Render dygraph
  output$plotobj <- dygraphs::renderDygraph({
    data    <- stratdata()
    retp    <- data$retp
    pnls    <- data$pnls
    posv    <- data$posv
    symboln <- data$symboln

    # Combine and downsample to weekly endpoints
    wealthv <- cbind(retp, pnls)
    endw    <- rutils::calc_endpoints(wealthv, interval="weeks")
    cumw    <- cumsum(wealthv)[endw]

    # Build plot title
    maintitle <- paste0(
      "Strategy for ", symboln,
      " / Index SR=",    data$sharpei,
      " / Strategy SR=", data$sharpes,
      " / Number of trades= ", data$ntrades
    )

    dygr <- dygraphs::dygraph(cumw, main=maintitle) %>%
      dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
      dyLegend(show="always", width=300)

    # Optional position shading: green = long, light-red = short
    if (input$shade == "True") {
      posvec <- as.numeric(posv[endw])
      datev  <- zoo::index(cumw)
      nw     <- length(datev)
      i <- 1
      while (i <= nw) {
        col <- if (posvec[i] > 0) "rgba(0,200,0,0.15)" else "rgba(200,0,0,0.10)"
        j <- i + 1
        while (j <= nw && posvec[j] == posvec[i]) j <- j + 1
        dygr <- dygr %>% dyShading(
          from  = as.character(datev[i]),
          to    = as.character(datev[min(j, nw)]),
          color = col)
        i <- j
      }
    }

    dygr
  })  # end renderDygraph

}  # end servfun


## Launch the app
shiny::shinyApp(ui=uifun, server=servfun)
