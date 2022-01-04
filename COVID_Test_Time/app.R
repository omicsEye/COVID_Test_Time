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
      "label{ display: table-cell; text-align: left; vertical-align: top; font-size:90%;
      font-weight:bold; white-space: normal;}
"
    )
  ),

  pageWithSidebar(
    headerPanel(title="COVID Testing: Modelling and Prediction",windowTitle="GW Modelling and Prediction"),
    #titlePanel('Model parameters'),
    #titlePanel('Population & model characteristics'),
    sidebarPanel(
      style = "overflow-y:scroll;max-height: 800px; position:relative;",
      h4("Planner inputs:"),
      width = 4,
      fluidRow(column(
        width = 12,
        actionButton(

          inputId = "resetinput",
          label = "Reset all to default")
      )),
      h4(""),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "n",
          label = "Population size (n):",
          value = n,
          min = 1000,step = 500
        )
      )),
      fluidRow(column(
        width = 12,
          numericInput(

            inputId = "test_frequency_UNP",
            label = "Test frequency for unprotected (one every ? days):",
            value = testfreq_UNP,
            min = 1,step=1
          )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "test_frequency_VAXP",
          label = "Test frequency for vaccinated \n(one every ? days):",
          value = testfreq_VAXP,
          min = 1,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "test_frequency_EVP",
          label = "Test frequency for exposed to \nolder variants (one every ? days):",
          value = testfreq_EVP,
          min = 1,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "test_frequency_CVP",
          label = "Test frequency for exposed to \ncurrent variant (one every ? days):",
          value = testfreq_CVP,
          min = 1,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "theta",
          label = "Days to incubation \n(Exposed-->Asymptomatic):",
          value = daystoincubation,
          min = 0,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "sigma",
          label = "% symptoms \n(Assymptomatic-->Symptomatic):",
          value = percenttosymptoms,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "rho",
          label = "Days to recovery",
          value = daystorecovery,
          min = 1,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "mu",
          label = "Return of FPs to \nthe Uninfected pool (days):",
          value = fptouninfpool,
          min = 1,step=1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "delta",
          label = "% fatality rate:",
          value = percentfatality,
          min = 0,max=1,step=0.001
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "Xshock",
          label = "Daily number of \ninported infections (Xshock):",
          value = Xshock,
          min = 0,step=10
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "nUNP",
          label = "% unprotected (nUNP):",
          value = nUNP,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "nVAXP",
          label = "% fully vaccinated (nVAXP):",
          value = nVAXP,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "nEVP",
          label = "% previously infected with \nolder variants (nEVP):",
          value = nEVP,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "nCVP",
          label = "% previously infected with \ncurrent variant (nCVP):",
          value = nCVP,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "AUNP0",
          label = "Initial % infected, \nasymptomatic, unprotected (AUNP0)",
          value = AUNP0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "AVAXP0",
          label = "Initial % infected, \nasymptomatic, vaccinated (AVAXP0)",
          value = AVAXP0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "AEVP0",
          label = "% Initial, infected, asymptomatic, \nprevious older variant infection (AEVP0):",
          value = AEVP0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "ACVP0",
          label = "% Initial, infected, asymptomatic, \nprevious current variant infection (ACVP0):",
          value = ACVP0,
          min = 0,max=1,step=0.1
        )
      )),

      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "R0",
          label = "R0:",
          value = R0,
          min = 0,step=0.2
        )
      )),

      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_VAXi0",
          label = "Preventive efficacy for infection \nat t0 (epsilon_VAXi0):",
          value = epsilon_VAXi0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_VAXi6m",
          label = "Preventive efficacy for infection \nfrom vaccine at 6M (epsilon_VAXi6m):",
          value = epsilon_VAXi6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_VAXt0",
          label = "Preventive efficacy for transmission \nfrom vaccine at t0 (epsilon_VAXt0):",
          value = epsilon_VAXt0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_VAXt6m",
          label = "Preventive efficacy for transmission \nfrom vaccine at 6M (epsilon_VAXt6m):",
          value = epsilon_VAXt6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_EVi0",
          label = "Preventive efficacy for infection \nfrom past older variants at t0 (epsilon_EVi0):",
          value = epsilon_EVi0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_EVi6m",
          label = "Preventive efficacy for infection \nfrom past older variants at 6M (epsilon_EVi6)::",
          value = epsilon_EVi6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_EVt0",
          label = "Preventive efficacy for transmission \nfrom past older variants at t0 (epsilon_EVt0):",
          value = epsilon_EVt0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_EVt6m",
          label = "Preventive efficacy for transmission \nfrom past older variants at 6M (epsilon_EVt6m):",
          value = epsilon_EVt6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_CVi0",
          label = "Preventive efficacy for infection \nfrom past current variants at t0 (epsilon_CVi0):",
          value = epsilon_CVi0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_CVi6m",
          label = "Preventive efficacy for infection \nfrom past current variants at 6M (epsilon_CVi6m):",
          value = epsilon_CVi6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_CVt0",
          label = "Preventive efficacy for transmission \nfrom past older variants at t0 (epsilon_CVt0):",
          value = epsilon_CVt0,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "epsilon_CVt6m",
          label = "Preventive efficacy for transmission \nfrom older variants at 6M (epsilon_CVt6m):",
          value = epsilon_CVt6m,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "Se",
          label = "Sensitivity:",
          value = Se,
          min = 0,max=1,step=0.1
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "Sp",
          label = "Specificity:",
          value = Sp,
          min = 0,max=1,step=0.01
        )
      )),
      fluidRow(column(
        width = 12,
        numericInput(

          inputId = "ncycles",
          label = "Number of cycles:",
          value = ncycles,
          min = 0,step=10
        )
      ))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      tabsetPanel(
        tabPanel("Cumulative on isolation", plotlyOutput(outputId = "accumlative")),
        tabPanel("New infected", plotlyOutput(outputId = "newInf")),
        tabPanel(
          "Custom plots",
          fluidPage(
            tags$style(HTML("
                       .selectize-input {
                         height: 40px;
                         width: 130px;
                         font-size: 12pt;
                        padding-top: 5px;
                          }
                   ")),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "xfield",
                  label = "Select field to plot on x:",
                  choices = names(params),
                  selected = 'Xshock'
                )),
              column(
                width = 4,
                selectInput(
                  inputId = "yfield",
                  label = "Select field to plot on y:",
                  choices = c(names(results)),
                  selected = 'inisolation'
                )),
              column(
                width = 2,
                selectInput(
                  inputId = "fun",
                  label = "Select function:",
                  choices = c('min','max','mean','median','peaktime'),
                  selected = 'max'
                )),
              column(
                width = 4,
                numericInput(

                  inputId = "lower",
                  label = "Select lower limit:",
                  value=0,
                  min=0,
                )),
              column(
                width = 4,
                numericInput(

                  inputId = "upper",
                  label = "Select upper limit:",
                  value=1000,
                  min=0
                )),
              column(
                width = 4,
                numericInput(

                  inputId = "steps",
                  label = "Select number of steps:",
                  value=100,max=300,min=2
                ))
            ), # end fluidRow
            fluidRow(plotlyOutput('customplot'))
          ) # end fluidPage
        ), # end Custom plots tab
        tabPanel("Output Data Frame", DT::dataTableOutput("results"))
      ) # end tabsetpanel
    ) # end Main panel
  ) # end pageWithSidebar
) # end ui



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
  ### end JS code for reset defaults



    params <- reactive(list(
      n=input$n,
      testfreq_UNP=input$test_frequency_UNP,
      testfreq_VAXP=input$test_frequency_VAXP,
      testfreq_EVP=input$test_frequency_EVP,
      testfreq_CVP=input$test_frequency_CVP,

      nUNP=input$nUNP,
      nVAXP=input$nVAXP,
      nEVP=input$nEVP,
      nCVP=input$nCVP,
      AUNP0=input$AUNP0,
      AVAXP0=input$AVAXP0,
      AEVP0=input$AEVP0,
      ACVP0=input$ACVP0,

      ncycles=input$ncycles,
      daystoincubation=input$theta,
      percenttosymptoms=input$sigma,
      daystorecovery=input$rho,
      R0=input$R0 , ### reproduction rate,
      # beta <- Rstar * (sigma + rho)
      percentfatality=input$delta,
      fptouninfpool=input$mu,

      epsilon_VAXi0=input$epsilon_VAXi0,
      epsilon_VAXi6m=input$epsilon_VAXi6m,
      epsilon_VAXt0=input$epsilon_VAXt0,
      epsilon_VAXt6m=input$epsilon_VAXt6m,

      epsilon_EVi0=input$epsilon_EVi0,
      epsilon_EVi6m=input$epsilon_EVi6m,
      epsilon_EVt0=input$epsilon_EVt0,
      epsilon_EVt6m=input$epsilon_EVt6m,

      epsilon_CVi0=input$epsilon_CVi0,
      epsilon_CVi6m=input$epsilon_CVi6m,
      epsilon_CVt0=input$epsilon_CVt0,
      epsilon_CVt6m=input$epsilon_CVt6m,

      freqShock=freqShock,
      Xshock=input$Xshock,
      #test_frequency <- input$test_frequency
      # number_of_tests <- input$ncycles / input$test_frequency
      # testfreq_UNP <- 1.0 / number_of_tests ### weekly testing
      # testfreq_VAXP <- 1.0 / number_of_tests
      # testfreq_EVP <- 1.0 / number_of_tests
      # testfreq_CVP <- 1.0 / number_of_tests
      Se=input$Se,
      Sp=input$Sp
      # mu <- input$mu
    ))

    #  prediction_results <- do.call(covidpred,params)
    # # n, nUNP, nVAXP, nEVP, nCVP, AUNP0, AVAXP0, AEVP0, ACVP0,
    # ncycles, daystoincubation, daystorecovery, percenttosymptoms,
    # fptouninfpool, percentfatality, R0,
    # epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
    # epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
    # epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
    # freqShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
    # Se, Sp)
    # results <- list()
    # results$Data <- data.table(prediction_results)
    # results$n<- n
    # results$ncycles <- ncycles
    # # results$Data[,inisolation:=mapply(sum,M,FPVAXP,TPVAXP,TPCVP,TPEVP,FPEVP,FPCVP,TPUNP,FPUNP)]
    # return(results)
  # })
# results
# isolate(print(read_input()))
output$accumlative <- renderPlotly({
  results <-  do.call(covidpred,params())
  results[,day:=.I]
  ggplotly(
    ggplot2::ggplot(results,
                    aes(x=day,
                        y=inisolation)) +
      ggplot2::geom_line(size = .5) +
      ggplot2::geom_abline(slope=0,intercept=300, col='red', size = .25) +
      ggplot2::geom_hline(yintercept = 0,
                          color = "black",
                          size = 0.1) +
      ggplot2::xlab ("Days") +
      ggplot2::ylab ("Total number of individuals in isolation") +
      ggplot2::ggtitle ("Total number of individuals in isolation each day") +
      theme_omicsEye_presentation()
  )
})
output$newInf <- renderPlotly({
  results <-  do.call(covidpred,params())
  results[,day:=.I]
  p <- ggplot2::ggplot(results, aes(
    x=day, y=newinf)) + # *results$n/100)) +  # i think newinf is actual number of new infections
    ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "black",
                        size = 0.1) +
    ggplot2::xlab ("Days") +
    # ggplot2::ylab ("newinf*n/100") +
    ggplot2::ylab('Number of new infections per day') +
    ggplot2::ggtitle ("Newly infected individuals") +
    theme_omicsEye_presentation()
  ggplotly(p)
})
output$results <- DT::renderDataTable({
  results <-  do.call(covidpred,params())
  DT::datatable(results, options = list(lengthMenu = c(10, 30, params()$ncycles), pageLength = 5))
})

output$customplot <- renderPlotly({
  x = seq(from=input$lower,to=input$upper,by=(input$upper-input$lower)/input$steps)
  dt <- data.table(x)
  fun=input$fun
  for(i in x) {
    # print(i)
    pars <- params()
    pars[[input$xfield]] <- i
    tmp <- data.table(do.call(covidpred,pars))
    if(fun=='peaktime') {
      fun = 'which.max'
    }
    yi <- tmp[,.(y=get(input$yfield))][,.(y=get(fun)(y))]$y
    dt[x==i,y:=yi]
  }
  setnames(dt,c('x','y'),c(input$xfield,input$yfield))
  # print(dt)
  dt[,xfield:=paste0(
    input$xfield, " = ", round(get(input$xfield),1),'\n',
    fun,' of ', input$yfield, " = ", round(get(input$yfield),1)
  )]
  ggplotly(
    ggplot(dt,aes(get(input$xfield),get(input$yfield),label=xfield))+geom_line(method = "glm",
                                                                               size = .5,
                                                                               color = 'black',alpha = .75,)+
                                                                      geom_point(fill = 'darkolivegreen4',
                                                                                            color = 'black',
                                                                                            alpha = .5,
                                                                                            shape = 21,
                                                                                            size = 1.75,
                                                                                            stroke = 0.25)+
      xlab(input$xfield)+
      ylab(paste("Function:",fun,"of",input$yfield)) + theme_omicsEye_presentation(),
    tooltip=c('label')
  )
}) # end customplot
}
shinyApp(ui = ui, server = server)
