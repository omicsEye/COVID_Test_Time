library(shiny)
library(DT)
library(ggplot2)
source("model.R", local = TRUE)
source("utils.R", local = TRUE)
# Define UI for app that draws a histogram ----
ui <- fluidPage(# App title ----
                #headerPanel(title="Modelling and Prediction",windowTitle="GW Modelling and Prediction"),
                #titlePanel("GW COVID-19 Time to Test"),
                tags$head(
                  tags$style(
                    type = "text/css",
                    "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}"
                  )
                ),
                pageWithSidebar(
                  headerPanel(title = "COVID Testing: Modelling and Prediction", windowTitle =
                                "GW Modelling and Prediction"),
                  #titlePanel('Model parameters'),
                  #titlePanel('Population & model characteristics'),
                  sidebarPanel(
                    h4("Planner inputs:"),
                    width = 4,
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "n",
                        label = "n:",
                        value = 25000,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "nUNP",
                        label = "nUNP:",
                        value = 625,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "nVAXP",
                        label = "nVAXP:",
                        value = 20000,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "nEVP",
                        label = "nEVP:",
                        value = 1250,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "nCVP",
                        label = "nCVP:",
                        value = 3125,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "AUNP0",
                        label = "AUNP0:",
                        value = 25,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "AVAXP0",
                        label = "AVAXP0:",
                        value = 1000,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "AEVP0",
                        label = "AEVP0:",
                        value = 50,
                        min = 1
                      )
                    )),

                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "ACVP0",
                        label = "ACVP0:",
                        value = 0,
                        min = 1
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "daystoincubation",
                        label = "daystoincubation:",
                        value = 3,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "daystorecovery",
                        label = "daystorecovery:",
                        value = 10,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "percenttosymptoms",
                        label = "percenttosymptoms:",
                        value = .3,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "fptouninfpool",
                        label = "fptouninfpool:",
                        value = 1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "percentfatality",
                        label = "percentfatality:",
                        value = 0.0005,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "R0",
                        label = "R0:",
                        value = 3,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "testfreq_UNP",
                        label = "testfreq_UNP:",
                        value = 7,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "testfreq_VAXP",
                        label = "testfreq_VAXP:",
                        value = 7,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "testfreq_EVP",
                        label = "testfreq_EVP:",
                        value = 7,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "testfreq_CVP",
                        label = "testfreq_CVP:",
                        value = 7,
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
                        value = 7,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "Xshock",
                        label = "Xshock:",
                        value = 25,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_VAXi0",
                        label = "epsilon_VAXi0:",
                        value = 0.5,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_VAXi6m",
                        label = "epsilon_VAXi6m:",
                        value = 0.2,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_VAXt0",
                        label = "epsilon_VAXt0:",
                        value = 0.1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_VAXt6m",
                        label = "epsilon_VAXt6m:",
                        value = 0.1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_EVi0",
                        label = "epsilon_EVi0:",
                        value = 0.5,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_EVi6m",
                        label = "epsilon_EVi6m:",
                        value = 0.2,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_EVt0",
                        label = "epsilon_EVt0:",
                        value = .1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_EVt6m",
                        label = "epsilon_EVt6m:",
                        value = 0.1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_CVi0",
                        label = "epsilon_CVi0:",
                        value = 1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_CVi6m",
                        label = "epsilon_CVi6m:",
                        value = 1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_CVt0",
                        label = "epsilon_CVt0:",
                        value = 1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "epsilon_CVt6m",
                        label = "epsilon_CVt6m:",
                        value = 1,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "Se",
                        label = "Se:",
                        value = 0.99,
                        min = 0
                      )
                    )),
                    fluidRow(column(
                      width = 10,
                      numericInput(
                        "num",
                        inputId = "Sp",
                        label = "Sp:",
                        value = 0.99,
                        min = 0
                      )
                    ))
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(# Output: Histogram ----
                            tabsetPanel(
                              tabPanel("Accumlative", plotOutput(outputId = "accumlative")),
                              tabPanel("New infected", plotOutput(outputId = "newInf")),
                              tabPanel("Output Data Frame", DT::dataTableOutput("results"))
                              #tabPanel("plot", plotOutput(outputId = "distPlot"))
                            ))
                ))



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
    nUNP <- input$nUNP ### 2.5% unprotected
    nVAXP <- input$nVAXP
    nEVP <- input$nEVP
    nCVP <- input$nCVP
    AUNP0 <- input$AUNP0
    AVAXP0 <- input$AVAXP0
    AEVP0 <- input$AEVP0
    ACVP0 <- input$ACVP0

    daystoincubation <- input$daystoincubation
    daystorecovery <- input$daystorecovery
    percenttosymptoms <- input$percenttosymptoms
    fptouninfpool <- input$fptouninfpool
    percentfatality <- input$percentfatality
    R0 <- input$R0
    ncycles <- input$ncycles

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
    testfreq_UNP <- input$testfreq_UNP ### weekly testing
    testfreq_VAXP <- input$testfreq_VAXP
    testfreq_EVP <- input$testfreq_EVP
    testfreq_CVP <- input$testfreq_CVP
    Se <- input$Se
    Sp <- input$Sp
    mu <- input$mu

    prediction_results <-
      covidpred(
        n,
        nUNP,
        nVAXP,
        nEVP,
        nCVP,
        AUNP0,
        AVAXP0,
        AEVP0,
        ACVP0,
        ncycles,
        daystoincubation,
        daystorecovery,
        percenttosymptoms,
        fptouninfpool,
        percentfatality,
        R0,
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
        testfreq_UNP,
        testfreq_VAXP,
        testfreq_EVP,
        testfreq_CVP,
        Se,
        Sp
      )

    results <- list()
    results$Data <- as.data.frame(prediction_results)
    results$n <- n
    results$ncycles <- ncycles
    return(results)
  }
  output$accumlative <- renderPlot({
    results <-  read_input()
    results_data <- results$Data
    ggplot2::ggplot(results_data,
                    aes(
                      x = seq(rownames(results_data)),
                      y =cumnewinf
                    )) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0,
                          color = "red",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("Cumulative number of new infections") +
      ggplot2::ggtitle ("Cumulative number of new infections") +
      theme_omicsEye_presentation()
  })
  output$newInf <- renderPlot({
    results <-  read_input()
    results_data <- results$Data
    ggplot2::ggplot(results_data, aes(x = seq(rownames(results_data)), y =
                                        newinf)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0,
                          color = "red",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("Daily new infections") +
      ggplot2::ggtitle ("Daily new infections") +
      theme_omicsEye_presentation()
  })
  output$results <- DT::renderDataTable({
    results <-  read_input()
    results_data <- results$Data
    DT::datatable(results_data, options = list(
      lengthMenu = c(10, 30, results$ncycles),
      pageLength = 5
    ))
  })

}

shinyApp(ui = ui, server = server)
