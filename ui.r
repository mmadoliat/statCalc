library(shiny)

options(shiny.sanitize.errors = FALSE)

shinyUI(fluidPage(
  titlePanel("JAMM - Stat Calculator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        'input.Panel === "Data"',
        radioButtons("f.choice", "Choose from:", c("Server" = "server", "Upload" = "upload"), selected = "upload", inline = TRUE, width = "250px"),
        uiOutput("which.server", width = "350px"), uiOutput("s.choice", width = "250px"), uiOutput("file"), uiOutput("sep"), uiOutput("header")
      ),
      conditionalPanel(
        'input.Panel === "Descriptive Statistics"',
        checkboxInput("i2ndVar", "Include 2nd Variable", value = FALSE),
        uiOutput("var1", width = "300px"), uiOutput("var2", width = "300px"), uiOutput("desFig", width = "300px"), uiOutput("nbin", width = "300px"),
        uiOutput("quant", width = "300px"), checkboxInput("insumstats", "Include Summary Statistics", value = TRUE)
      ),
      conditionalPanel(
        'input.Panel === "One Sample"',
        uiOutput("svar", width = "300px"), uiOutput("type", width = "300px"), uiOutput("dif.tst", width = "300px"), uiOutput("sigma.squared", width = "300px"),
        uiOutput("fctpptn", width = "300px"), uiOutput("HT_plts", width = "300px"), uiOutput("confpval", width = "300px"),
        sliderInput("conflev", HTML("Conf. Level (1- &alpha;)"), min = 0.5, max = 0.99, value = 0.95, step = .01, width = "150px"),
        uiOutput("nullhypo", width = "300px"), uiOutput("althypo", width = "300px"),
      ),
      conditionalPanel(
        'input.Panel === "One + Sample"',
        uiOutput("oneplusvar1", width = "300px"), uiOutput("oneplusvar2", width = "300px"), uiOutput("oneplustype", width = "300px"),
        uiOutput("dependency", width = "300px"), uiOutput("var.eq", width = "300px"), uiOutput("confintp", width = "300px"),
        sliderInput("conflevp", HTML("Conf. Level (1- &alpha;)"), min = 0.5, max = 0.99, value = 0.95, step = .01, width = "150px"),
        uiOutput("pnullhypo", width = "300px"), uiOutput("palthypo", width = "50px"),
        uiOutput("fctpptn1", width = "300px"), uiOutput("fctpptn2", width = "300px"),
      )
    ),
    mainPanel(
      width = 9, tags$style(type = "text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; },"), # ".nav-tabs {font-size: 10px}"),
      tabsetPanel(
        id = "Panel", type = "tabs",
        tabPanel(
          title = "Data", value = "Data",
          column(12, uiOutput("ts.selected", align = "center"), style = "color:blue;"),
          column(6, plotOutput("data.plot", height = 600, width = 600)), column(6, DT::dataTableOutput("dynamic"))
        ),
        tabPanel(
          title = "Descriptive Statistics",
          column(12, plotOutput("des.plot", height = 600, width = 600)),
          fluidRow(verbatimTextOutput("des.summ"))
        ),
        tabPanel(
          title = "One Sample",
          fluidRow(column(12, plotOutput("onesamtst.plt", height = 600, width = 600))),
          fluidRow(verbatimTextOutput("onesamtst"))
        ),
        tabPanel(
          title = "One + Sample",
          fluidRow(column(12, plotOutput("oneplussamtst.plt", height = 600, width = 600))),
          fluidRow(verbatimTextOutput("oneplussamtst"))
        )
      )
    )
  )
))