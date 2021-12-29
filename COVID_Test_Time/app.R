library(shiny)
library(ggplot2)
source("model.R", local = TRUE)
source("utils.R", local = TRUE)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("GW COVID-19 Time to Test Modelling and Prediction!"),
  tags$head(
    tags$style(
      type = "text/css",
      "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}"
    )
  ),
  pageWithSidebar(
    titlePanel('Population & model characteristics'),
    sidebarPanel(
      width = 4,
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "n",
          label = "n:",
          value = 25000
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "test_frequency",
          label = "test_frequency:",
          value = 7
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "E0",
          label = "E0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "A0",
          label = "A0:",
          value = 25
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "M0",
          label = "M0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TP0",
          label = "TP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FP0",
          label = "FP0:",
          value = 0
        )
      )),

      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "EVAXP0",
          label = "EVAXP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "AVAXP0",
          label = "AVAXP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPVAXP0",
          label = "TPVAXP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPVAXP0",
          label = "FPVAXP0:",
          value = 2.5
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "EEVP0",
          label = "EEVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "AEVP0",
          label = "AEVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPEVP0",
          label = "TPEVP0:",
          value = 2.5
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPEVP0",
          label = "FPEVP0:",
          value = 2.5
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ECVP0",
          label = "ECVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ACVP0",
          label = "ACVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPCVP0",
          label = "TPCVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPCVP0",
          label = "FPCVP0:",
          value = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ncycles",
          label = "ncycles:",
          value = 120
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "theta",
          label = "theta:",
          value = 1.0 / 3.0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "sigma",
          label = "sigma:",
          value = 0.031
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "rho",
          label = "rho:",
          value = 0.071
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Rstar",
          label = "Rstar:",
          value = 0.071
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "delta",
          label = "delta:",
          value = 0.0000357
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXi0",
          label = "epsilon_VAXi0:",
          value = 0.85
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXi6m",
          label = "epsilon_VAXi6m:",
          value = 0.66
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXt0",
          label = "epsilon_VAXt0:",
          value = 0.25
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXt6m",
          label = "epsilon_VAXt6m:",
          value = 0.25
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVi0",
          label = "epsilon_EVi0:",
          value = 0.85
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVi6m",
          label = "epsilon_EVi6m:",
          value = 0.66
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVt0",
          label = "epsilon_EVt0:",
          value = .50
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVt6m",
          label = "epsilon_EVt6m:",
          value = 0.25
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVi0",
          label = "epsilon_CVi0:",
          value = 1
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVi6m",
          label = "epsilon_CVi6m:",
          value = 1
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVt0",
          label = "epsilon_CVt0:",
          value = 1
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVt6m",
          label = "epsilon_CVt6m:",
          value = 1
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "freqShock",
          label = "freqShock:",
          value = 7
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Xshock",
          label = "Xshock:",
          value = 5
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Se",
          label = "Se:",
          value = 0.9
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Sp",
          label = "Sp:",
          value = 0.95
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "mu",
          label = "mu:",
          value = 1
        )
      ))
    ),
    # Main panel for displaying outputs ----
    mainPanel(# Output: Histogram ----
              tabsetPanel(
                tabPanel("Accumlative", plotOutput(outputId = "accumlative")),
                tabPanel("New infected", plotOutput(outputId = "newInf"))
                #tabPanel("plot", plotOutput(outputId = "distPlot"))
              ))
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # output$distPlot <- renderPlot({
  #
  #   ggplot2::ggplot(calculation_data_long, aes(R, rate, color = Testing)) +
  #     ggplot2::geom_line() +
  #     ggplot2::geom_hline(yintercept = 0.05, color = "red", size = 0.1) +
  #     ggplot2::xlab ("On-Campus Effective Reproduction Number\n(Pre-vaccination, pre-testing, post-NPIs)")+
  #     ggplot2::ylab ("New Infections (as a % of  poppulation)")+
  #     ggplot2::ggtitle ("Effect of different Vaxxed Testing Frequencies\nHolding the Unvaxxed Frequency Constant")+
  #     omicsArt::theme_omicsEye_presentation()
  # })
  read_input <- function() {
    n <- input$n
    E0 <- input$E0
    A0 <- input$A0
    U0 <- 0.025 * n - A0
    M0 <- input$M0
    TP0 <- input$TP0
    FP0 <- input$FP0
    UVAXP0 <- 0.8 * n
    EVAXP0 <- input$EVAXP0
    AVAXP0 <- input$AVAXP0
    TPVAXP0 <- input$TPVAXP0
    FPVAXP0 <- input$FPVAXP0
    UEVP0 <- 0.05 * n
    EEVP0 <- input$EEVP0
    AEVP0 <- input$AEVP0
    TPEVP0 <- input$TPEVP0
    FPEVP0 <- input$FPEVP0
    UCVP0 <- 0.125 * n
    ECVP0 <- input$ECVP0
    ACVP0 <- input$ACVP0
    TPCVP0 <- input$TPCVP0
    FPCVP0 <- input$FPCVP0

    ncycles <- input$ncycles
    theta <- input$theta
    sigma <- input$sigma
    rho <- input$rho
    Rstar <- input$Rstar ### reproduction rate
    beta <- Rstar * (sigma + rho)
    delta <- input$delta
    epsilon_VAXi0 <- input$epsilon_VAXi0
    epsilon_VAXi6m <- input$epsilon_VAXi6m
    epsilon_VAXt0 <- input$epsilon_VAXt0
    epsilon_VAXt6m <- input$epsilon_VAXt6m

    epsilon_EVi0 <- input$epsilon_EVi0
    epsilon_EVi6m <- input$epsilon_EVi6m
    epsilon_EVt0 <- input$epsilon_EVt0
    epsilon_EVt6m <- input$epsilon_EVt6m

    epsilon_CVi0 <- input$epsilon_CVi0
    epsilon_CVi6m <- input$epsilon_CVi6m
    epsilon_CVt0 <- input$epsilon_CVt0
    epsilon_CVt6m <- input$epsilon_CVt6m

    freqShock <- input$freqShock
    Xshock <- input$Xshock
    test_frequency <- input$test_frequency
    tau <- 1.0 / test_frequency ### weekly testing
    tau_VAXP <- 1.0 / test_frequency
    tau_EVP <- 1.0 / test_frequency
    tau_CVP <- 1.0 / test_frequency
    Se <- input$Se
    Sp <- input$Sp
    mu <- input$mu

    results <- covidpred(
      U0,
      E0,
      A0,
      M0,
      TP0,
      FP0,
      UVAXP0,
      EVAXP0,
      AVAXP0,
      TPVAXP0,
      FPVAXP0,
      UEVP0,
      EEVP0,
      AEVP0,
      TPEVP0,
      FPEVP0,
      UCVP0,
      ECVP0,
      ACVP0,
      TPCVP0,
      FPCVP0,
      ncycles,
      beta,
      epsilon_VAXt0,
      epsilon_VAXt6m,
      epsilon_VAXi0,
      epsilon_VAXi6m,
      epsilon_EVt0,
      epsilon_EVt6m,
      epsilon_EVi0,
      epsilon_EVi6m,
      epsilon_CVt0,
      epsilon_CVt6m,
      epsilon_CVi0,
      epsilon_CVi6m,
      freqShock,
      Xshock,
      tau,
      tau_VAXP,
      tau_EVP,
      tau_CVP,
      Se,
      Sp,
      mu,
      theta,
      sigma,
      rho,
      delta
    )
    results <- as.data.frame(results)
    return(results)
  }
  output$accumlative <- renderPlot({
    results <-  read_input()
    ggplot2::ggplot(results,
                    aes(M + FP + TP + TPCVP + TPEVP + TPVAXP + FPEVP + FPCVP, seq(rownames(results)))) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0.05,
                          color = "red",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("Cummulative number of infected indivituals") +
      ggplot2::ggtitle ("") +
      theme_omicsEye_presentation()
  })
  output$newInf <- renderPlot({
    results <-  read_input()
    ggplot2::ggplot(results, aes(newinf, seq(rownames(results)))) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0.05,
                          color = "red",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("New infected indivituals") +
      ggplot2::ggtitle ("") +
      theme_omicsEye_presentation()
  })

}

shinyApp(ui = ui, server = server)
