library(shiny)
library(DT)
library(ggplot2);library(plotly)
library(data.table)
source("model.R", local = TRUE)
source("utils.R", local = TRUE)
source("user_parameters.R")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  #headerPanel(title="Modelling and Prediction",windowTitle="GW Modelling and Prediction"),
  #titlePanel("GW COVID-19 Time to Test"),
  tags$head(
    tags$style(
      type = "text/css",
      "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}"
    )
  ),
  pageWithSidebar(
    headerPanel(title="COVID Testing: Modelling and Prediction",windowTitle="GW Modelling and Prediction"),
    #titlePanel('Model parameters'),
    #titlePanel('Population & model characteristics'),
    sidebarPanel(
      h4("Planner inputs:"),
      width = 4,
      fluidRow(column(
        width = 10,
        actionButton(
          
          inputId = "resetinput",
          label = "Reset all to default")
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "n",
          label = "n:",
          value = n,
          min = 1
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "test_frequency",
          label = "test_frequency (one every ? days):",
          value = test_frequency,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "theta",
          label = "theta:",
          value = theta,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "sigma",
          label = "sigma:",
          value = sigma,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",   
          inputId = "rho",
          label = "rho:",
          value = rho,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ncycles",
          label = "ncycles:",
          value = 120,
          min = 0
        )
      )),
      h4("Imported infections:"),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "freqShock",
          label = "freqShock:",
          value = freqShock,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Xshock",
          label = "Xshock:",
          value = Xshock,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "E0",
          label = "E0:",
          value = E0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "A0",
          label = "A0:",
          value = A0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "M0",
          label = "M0:",
          value = M0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TP0",
          label = "TP0:",
          value = TP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FP0",
          label = "FP0:",
          value = FP0,
          min = 0
        )
      )),

      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "EVAXP0",
          label = "EVAXP0:",
          value = EVAXP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "AVAXP0",
          label = "AVAXP0:",
          value = AVAXP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPVAXP0",
          label = "TPVAXP0:",
          value = TPVAXP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPVAXP0",
          label = "FPVAXP0:",
          value = FPVAXP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "EEVP0",
          label = "EEVP0:",
          value = EEVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "AEVP0",
          label = "AEVP0:",
          value = AEVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPEVP0",
          label = "TPEVP0:",
          value = TPEVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPEVP0",
          label = "FPEVP0:",
          value = FPEVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ECVP0",
          label = "ECVP0:",
          value = ECVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "ACVP0",
          label = "ACVP0:",
          value = ACVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "TPCVP0",
          label = "TPCVP0:",
          value = TPCVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "FPCVP0",
          label = "FPCVP0:",
          value = FPCVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Rstar",
          label = "Rstar:",
          value = Rstar,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "delta",
          label = "delta:",
          value = delta,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXi0",
          label = "epsilon_VAXi0:",
          value = epsilon_VAXi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXi6m",
          label = "epsilon_VAXi6m:",
          value = epsilon_VAXi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXt0",
          label = "epsilon_VAXt0:",
          value = epsilon_VAXt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_VAXt6m",
          label = "epsilon_VAXt6m:",
          value = epsilon_VAXt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVi0",
          label = "epsilon_EVi0:",
          value = epsilon_EVi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVi6m",
          label = "epsilon_EVi6m:",
          value = epsilon_EVi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVt0",
          label = "epsilon_EVt0:",
          value = epsilon_EVt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_EVt6m",
          label = "epsilon_EVt6m:",
          value = epsilon_EVt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVi0",
          label = "epsilon_CVi0:",
          value = epsilon_CVi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVi6m",
          label = "epsilon_CVi6m:",
          value = epsilon_CVi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVt0",
          label = "epsilon_CVt0:",
          value = epsilon_CVt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "epsilon_CVt6m",
          label = "epsilon_CVt6m:",
          value = epsilon_CVt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Se",
          label = "Sensitivity:",
          value = Se,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "Sp",
          label = "Specificity:",
          value = Sp,
          min = 0
        )
      )),
      fluidRow(column(
        width = 10,
        numericInput(
          "num",
          inputId = "mu",
          label = "mu:",
          value = mu,
          min = 0
        )
      ))
    ),
    # Main panel for displaying outputs ----
    mainPanel(# Output: Histogram ----
              tabsetPanel(
                tabPanel("Cumulative on isolation", plotlyOutput(outputId = "accumlative")),
                tabPanel("New infected", plotlyOutput(outputId = "newInf")),
                 tabPanel("Output Data Frame", DT::dataTableOutput("results"))
                #tabPanel("plot", plotOutput(outputId = "distPlot"))
              ))
  )
)



# Define server logic required to draw a histogram ----
server <- function(session, input, output) {
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
  ### JS: Code to reset to default inputs
  initialInputs <- isolate(reactiveValuesToList(input))
  observe({
    # OPTIONAL - save initial values of dynamic inputs
    inputValues <- reactiveValuesToList(input)
    initialInputs <<- utils::modifyList(inputValues, initialInputs)
  })
  observeEvent(input$resetinput, {
    for (id in names(initialInputs)) {
      value <- initialInputs[[id]]
      # For empty chec`kboxGroupInputs
      if (is.null(value)) value <- ""
      session$sendInputMessage(id, list(value = value))
    }
  })
  ### end JS code
  
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
    #test_frequency <- input$test_frequency
    number_of_tests <- input$ncycles / input$test_frequency
    tau <- 1.0 / number_of_tests ### weekly testing
    tau_VAXP <- 1.0 / number_of_tests
    tau_EVP <- 1.0 / number_of_tests
    tau_CVP <- 1.0 / number_of_tests
    Se <- input$Se
    Sp <- input$Sp
    mu <- input$mu

    prediction_results <- covidpred(
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
    results <- list()
    results$Data <- as.data.frame(prediction_results)
    results$n<- n
    results$ncycles <- ncycles
    return(results)
  }
  output$accumlative <- renderPlotly({
    results <-  read_input()
    results_data <- results$Data
    ggplotly(
      ggplot2::ggplot(results_data,
                    aes(x=seq(rownames(results_data)),
                        y=M + FP + TP + TPCVP + TPEVP + TPVAXP + FPVAXP + FPEVP + FPCVP)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0,
                          color = "red",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("Total number of individuals in isolation") +
      ggplot2::ggtitle ("Total number of indivituals in isolation each day") # +
      # theme_omicsEye_presentation())
    )
  })
  output$newInf <- renderPlotly({
    results <-  read_input()
    results_data <- results$Data
    p <- ggplot2::ggplot(results_data, aes(
      x=seq(rownames(results_data)), y=newinf)) + # *results$n/100)) +  # i think newinf is actual number of new infections
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = 0,
                            color = "red",
                            size = 0.1) +
        ggplot2::xlab ("Days") +
        # ggplot2::ylab ("newinf*n/100") +
        ggplot2::ylab('Number of new infections per day') +
        ggplot2::ggtitle ("Newly infected indivituals") 
      # theme_omicsEye_presentation()
      ggplotly(p)
  })
  output$results <- DT::renderDataTable({
    results <-  read_input()
    results_data <- results$Data
    DT::datatable(results_data, options = list(lengthMenu = c(10, 30, results$ncycles), pageLength = 5))
  })

}

shinyApp(ui = ui, server = server)
