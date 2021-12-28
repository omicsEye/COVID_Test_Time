library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("GW COVID-19 Time to Test Modelling and Prediction!"),
  sidebarLayout(
    sidebarPanel(width = 12,
                 tags$head(
                   tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
                 ),
                 fluidRow(
                   column(width = 3,
                                 numericInput("num",
                                              inputId = "tau",
                                              label= "tau: rate at which non-antibody-protected in the testing pool are screened for infection:",
                                              value = 2.5))
                 ),
                 fluidRow(
                     column(width = 3,
                          numericInput("num",
                                       inputId = "tau_p",
                                       label= "tau_p: rate at which “antibody-protected” in the testing pool are screened for infection:",
                                       value = 97.5))
                  ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "mu",
                                       label= "mu: rate at which false positives are returned from FP and FPP to U and UP, respectively:",
                                       value = 0.0))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "theta",
                                       label= "theta: incubation, the rates at which exposed individuals in states E and EP advance to asymptomatic, infectious compartments A and AP, respectively:",
                                       value = 0.071))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "sigma",
                                       label= "sigma: the symptom onset rate from states A and TP to state M. It was assumed that 30% of persons with the asymptomatic infection would advance to symptoms:",
                                       value = 0.031))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "rho",
                                       label= "rho: rate at which individuals in state i recover from the disease:",
                                       value = 0.071))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "beta",
                                       label= "beta: rate at which infected individuals contact and infect susceptible individuals, i.e., transmission to persons in states E and EP (R*(sigma + rho) ):",
                                       value = 3.0))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "delta",
                                       label= "delta: the symptom-case fatality rate for individuals in state M:",
                                       value = 0.01))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "epsilon_i",
                                       label= "epsilon_i: antibody effectiveness in reducing susceptibility to infection:",
                                       value = 0.35))
                 ),
                 fluidRow(
                   column(width = 3,
                          numericInput("num",
                                       inputId = "epsilon_t",
                                       label= "epsilon_t: antibody effectiveness in reducing transmission:",
                                       value = 0.1))
                 )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(1, 100, length.out = 1000 + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "On-Campus Effective Reproduction Number\n(Pre-vaccination, pre-testing, post-NPIs)",
         ylab = "New Infections (as a % of  poppulation)",
         main = "Effect of different Vaxxed Testing Frequencies\nHolding the Unvaxxed Frequency Constant")

  })

}

shinyApp(ui = ui, server = server)
