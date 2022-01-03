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
        width = 12,
        actionButton(
          
          inputId = "resetinput",
          label = "Reset all to default")
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "n",
          label = "population size (n):",
          value = n,
          min = 1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "test_frequency_UNP",
          label = "test_frequency for unprotected (one every ? days):",
          value = testfreq_UNP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "test_frequency_VAXP",
          label = "test_frequency for vaccinated (one every ? days):",
          value = testfreq_VAXP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "test_frequency_EVP",
          label = "test_frequency for exposed to older variants (one every ? days):",
          value = testfreq_EVP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "test_frequency_CVP",
          label = "test_frequency for exposed to current variant (one every ? days):",
          value = testfreq_CVP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "theta",
          label = "Days to incubation (Exposed-->Asymptomatic):",
          value = daystoincubation,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "sigma",
          label = "Percent symptoms (Assymptomatic-->Symptomatic):",
          value = percenttosymptoms,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",   
          inputId = "rho",
          label = "Days to recovery",
          value = daystorecovery,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "mu",
          label = "Return of FPs to the Uninfected pool (days):",
          value = fptouninfpool,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "delta",
          label = "Percent fatality rate:",
          value = percentfatality,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "Xshock",
          label = "Daily number of inported infections (Xshock):",
          value = Xshock,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "nUNP",
          label = "Number of unprotected (nUNP):",
          value = nUNP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "nVAXP",
          label = "Number of fully vaccinated (nVAXP):",
          value = nVAXP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "nEVP",
          label = "Number previously infected with older variants (nEVP):",
          value = nEVP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "nCVP",
          label = "Number previously infected with current variant (nCVP):",
          value = nCVP,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "AUNP0",
          label = "Initial infected, asymptomatic, unprotected (AUNP0)",
          value = AUNP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "AVAXP0",
          label = "Initial infected, asymptomatic, vaccinated (AVAXP0)",
          value = AVAXP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "AEVP0",
          label = "Initial, infected, asymptomatic, previous older variant infection (AEVP0):",
          value = AEVP0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "ACVP0",
          label = "Initial, infected, asymptomatic, previous current variant infection (ACVP0):",
          value = ACVP0,
          min = 0
        )
      )),
      
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "R0",
          label = "R0:",
          value = R0,
          min = 0
        )
      )),
      
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_VAXi0",
          label = "Preventive efficacy for infection at t0 (epsilon_VAXi0):",
          value = epsilon_VAXi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_VAXi6m",
          label = "Preventive efficacy for infection from vaccine at 6M (epsilon_VAXi6m):",
          value = epsilon_VAXi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_VAXt0",
          label = "Preventive efficacy for transmission from vaccine at t0 (epsilon_VAXt0):",
          value = epsilon_VAXt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_VAXt6m",
          label = "Preventive efficacy for transmission from vaccine at 6M (epsilon_VAXt6m):",
          value = epsilon_VAXt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_EVi0",
          label = "Preventive efficacy for infection from past older variants at t0 (epsilon_EVi0):",
          value = epsilon_EVi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_EVi6m",
          label = "Preventive efficacy for infection from past older variants at 6M (epsilon_EVi6)::",
          value = epsilon_EVi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_EVt0",
          label = "Preventive efficacy for transmission from past older variants at t0 (epsilon_EVt0):",
          value = epsilon_EVt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_EVt6m",
          label = "Preventive efficacy for transmission from past older variants at 6M (epsilon_EVt6m):",
          value = epsilon_EVt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_CVi0",
          label = "Preventive efficacy for infection from past current variants at t0 (epsilon_CVi0):",
          value = epsilon_CVi0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_CVi6m",
          label = "Preventive efficacy for infection from past current variants at 6M (epsilon_CVi6m):",
          value = epsilon_CVi6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_CVt0",
          label = "Preventive efficacy for transmission from past older variants at t0 (epsilon_CVt0):",
          value = epsilon_CVt0,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "epsilon_CVt6m",
          label = "Preventive efficacy for transmission from older variants at 6M (epsilon_CVt6m):",
          value = epsilon_CVt6m,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "Se",
          label = "Sensitivity:",
          value = Se,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "Sp",
          label = "Specificity:",
          value = Sp,
          min = 0
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(
          "num",
          inputId = "ncycles",
          label = "Number of cycles:",
          value = ncycles,
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
    testfreq_UNP <- input$test_frequency_UNP
    testfreq_VAXP <- input$test_frequency_VAXP
    testfreq_EVP <- input$test_frequency_EVP
    testfreq_CVP <- input$test_frequency_CVP
    
    nUNP <- input$nUNP
    nVAXP <- input$nVAXP
    nEVP <- input$nEVP
    nCVP <- input$nCVP
    
    AUNP0 <- input$AUNP0
    AVAXP0 <- input$AVAXP0
    AEVP0 <- input$AEVP0
    ACVP0 <- input$ACVP0
    
    ncycles <- input$ncycles
    daysofincubation <- input$theta
    percenttosymptoms <- input$sigma
    daystorecovery <- input$rho
    R0 <- input$R0 ### reproduction rate
    # beta <- Rstar * (sigma + rho)
    percentfatality <- input$delta
    fptouninfpool <- input$mu
    
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
    
    
    
    freqShock <- freqShock
    Xshock <- input$Xshock
    #test_frequency <- input$test_frequency
    # number_of_tests <- input$ncycles / input$test_frequency
    # testfreq_UNP <- 1.0 / number_of_tests ### weekly testing
    # testfreq_VAXP <- 1.0 / number_of_tests
    # testfreq_EVP <- 1.0 / number_of_tests
    # testfreq_CVP <- 1.0 / number_of_tests
    Se <- input$Se
    Sp <- input$Sp
    # mu <- input$mu
    
    prediction_results <- covidpred(
      n, nUNP, nVAXP, nEVP, nCVP, AUNP0, AVAXP0, AEVP0, ACVP0, 
      ncycles, daystoincubation, daystorecovery, percenttosymptoms,
      fptouninfpool, percentfatality, R0, 
      epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
      epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
      epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
      freqShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
      Se, Sp)
    results <- list()
    results$Data <- as.data.frame(prediction_results)
    results$n<- n
    results$ncycles <- ncycles
    return(results)
  }
  isolate(print(read_input()))
  output$accumlative <- renderPlotly({
    results <-  read_input()
    results_data <- results$Data
    ggplotly(
      ggplot2::ggplot(results_data,
                      aes(x=seq(rownames(results_data)),
                          y=M + FPVAXP + TPVAXP + TPCVP + TPEVP + FPEVP + FPCVP)) +
        ggplot2::geom_line() +
        ggplot2::geom_abline(slope=0,intercept=300,col='red') +
        ggplot2::geom_hline(yintercept = 0,
                            color = "red",
                            size = 0.1) +
        ggplot2::xlab ("Days") +
        ggplot2::ylab ("Total number of individuals in isolation") +
        ggplot2::ggtitle ("Total number of individuals in isolation each day") # +
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
      ggplot2::ggtitle ("Newly infected individuals") 
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
