library(shiny)

ui <- fluidPage(
  titlePanel("Yield Curve Under the Vasicek Model"),
  fluidRow(
    column(3,
      sliderInput("ratet",
        label=strong("Short rate:"),
        min=0.1, max=6, value=4, step=0.1,
        ticks=TRUE)
    ),
    column(3,
      sliderInput("muv",
        label=strong("Equilibrium rate:"),
        min=0.1, max=7, value=2, step=0.1,
        ticks=TRUE)
    ),
    column(3,
      sliderInput("thetav",
        label=strong("Reversion strength:"),
        min=0.01, max=1, value=0.5, step=0.01,
        ticks=TRUE)
    ),
    column(3,
      sliderInput("sigmav",
        label=strong("Volatility:"),
        min=0.01, max=0.2, value=0.05, step=0.01,
        ticks=TRUE)
    )
  ),
  plotOutput("yieldPlot")
)

server <- function(input, output) {
  output$yieldPlot <- renderPlot({
    tauv   <- seq(1, 30, by=0.1)
    ratet  <- input$ratet
    muv    <- input$muv
    thetav <- input$thetav
    sigmav <- input$sigmav

    B <- (1 - exp(-thetav*tauv))/thetav
    A <- (muv - sigmav^2/(2*thetav^2))
    A <- A*(B - tauv) - (sigmav^2*B^2)/(4*thetav)
    ycurve <- (-A + B*ratet)/tauv

    plot(tauv, ycurve, type="l", lwd=3, col="blue",
      main="Yield Curve Under the Vasicek Model",
      xlab="Maturity (years)", ylab="Yield (%)")
  })
}

shinyApp(ui=ui, server=server)
