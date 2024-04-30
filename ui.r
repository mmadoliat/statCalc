library(shiny)
library(cluster)
library(shinythemes)
library(bslib)
library(heatmaply)
# test commit

options(shiny.sanitize.errors = FALSE)


shinyUI(
  fluidPage(
  shinythemes::themeSelector(), # R SHINY THEME #
  titlePanel("JAMM - Stat Calculator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        'input.Panel === "Data"',
        radioButtons("f.choice", "Choose from:", c("Server" = "server", "Upload" = "upload"), selected = "upload", inline = TRUE, width = "250px"),
        uiOutput("which.server", width = "350px"), uiOutput("s.choice", width = "250px"), uiOutput("file"), uiOutput("sep"), uiOutput("quote"), uiOutput("header")
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
      ),
      conditionalPanel(
        'input.Panel === "KMeans"',
        #fileInput("file1", "Choose CSV File", accept = ".csv"),
        uiOutput("varSelectUI"), # Dynamic UI for variable selection
        numericInput("clusters", "Number of Clusters:", 3, min = 2), 
        
        #Text to better understand KMeans
        br(),
        br(),
        p(strong("KMeans: "), 
          "A clustering algorithm that partions data into groups or 'clusters' based on similarity."),
        p(strong("Step 1: Initialization: "),
          "Choose the number of 'k's that you want. Randomly choose data points to act as the centriods of these 'k's. "),
        p(strong("Step 2: Assignment: "),
          "For each data point, calculate the distance between the point and each centroid. Assign the data point to the cluster whose centroid or cluster is closest to it."),
        p(strong("Step 3: Update data "),
          "Recalculate centriod based after all data points have been assigned. The centriod is the mean of the cluster."),
        p(strong("Step 4: Repeat: "),
          "The algorithm repeats steps 2 and 3 until centriods do not significally change."),
        p(strong("Step 5: Finalization: "),
          "Once algorithm has convereged and all data points are in their final cluster, everything is finalized")
      ),
      conditionalPanel(
        'input.Panel === "Anova"',
        #fileInput("file", "Upload input data (csv file with header)"),  
        
        uiOutput("yvarselect"),
        uiOutput("xvarselect"),
        uiOutput("fxvarselect"),
        
        # Text to better understand ANOVA table output and corresponding variables #
        br(),
        br(),
        p(strong("Estimate: "), 
          "calculates the sum of squared errors between the observed and predicted values of the outcome variable."),
        p(strong("Standard Error: "),
          "the accuracy of the estimated ordinary least squares (OLS) coefficient with respect to the population parameter."),
        p(strong("t-value: "),
          "the measurement of the difference relative to the variation in the data"),
        p(strong("Pr(>|t|): "),
          "p-value to determine whether to reject the null hypothesis or not.")
      )
    ),
    mainPanel(
      width = 9, tags$style(type = "text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; },"), # ".nav-tabs {font-size: 10px}"),
      tabsetPanel( 
        id = "Panel", type = "tabs",
        tabPanel(
          title = "Data", value = "Data",
          #column(12, uiOutput("ts.selected", align = "center"), style = "color:blue;"),
          column(12, plotOutput("data.plot", height = 600, width = 600)), 
          column(12, DT::dataTableOutput("dynamic"))
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
        ),
        tabPanel(
          title = "KMeans", 
          plotOutput("kmeansPlot"),
          fluidRow(column(12, plotOutput("plot1", height = 600, width = 600)))
        ),
        tabPanel(
          title = "Anova", 
          fluidRow(column(12, plotOutput("Plot1", height = 400, width = 600))),
          h4(p("Summary for selected Y variable(s):")),
          verbatimTextOutput('summaryY'), 
          h4(p("Summary for selected X Variable(s):")),
          DT::dataTableOutput('summaryX'),
          h4(p("OLS Results")),
          DT::dataTableOutput('OLSResult')),
        
        
        
        tabPanel(
          title = "Assumptions",
          
          br(),
          br(),
          "When performing simple or multuple linear regression, a few assumptions must be met.",
          br(),
          "This panel provides tests for multicollineariy and heteroskedasticity.",
          br(),
          br(),

          tabsetPanel(
            tabPanel(
              title = "Column Selection",
              br(),
              "Select which (numerical) variables to test data quality",
              br(),
              br(),
              
              selectInput(
                "var_select", label = NULL, choices = NULL, selected = NULL, multiple = TRUE),
              tableOutput("subset_contents")
              ),
            
            tabPanel(title = "Collinearity",
                     br(),
                     div(HTML("Collinearity exists when two <em>independent</em> varibles are correlated with one another.")),
                     br(),
                     div(HTML("When <em>multiple independent</em> varibles are correlated, multicolliearity exists.")),
                     br(),
                     "Any type of collinearity between independent variables is a violation of classical assumptions.",
                     br(),
                     br(),
                     
                     tabsetPanel(
                       tabPanel(title = "Correlation Heatmap",
                         br(),
                         "Correlation heatmaps display the degree of collinearity between variables.",
                         br(),
                         br(),
                         "Darker colors suggest higher correlation between the two varibles.",
                         br(),
                         br(),
                         plotlyOutput("heatmap_plot")
                       ),
                       tabPanel(title = "Variance Inflation Factos (VIFs)",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("vif_1_select", label = NULL, choices = NULL, selected = NULL, multiple = FALSE),
                             selectInput("vif_2_select", label = NULL, choices = NULL, selected = NULL, multiple = TRUE), 
                             br(),
                             "Important!",
                             br(),
                             br(),
                             "The first variable is not the dependent variable. If your models is: ",
                             br(),
                             "y = x1 + x2 + ... xi + c",
                             br(),
                             br(),
                             "then the first variable should be one of the independent varibles. Any combination of the remaining indepedent variable can be used for the second portion."
                           ),
                           mainPanel(
                             br(),
                             verbatimTextOutput("vif_output"),
                             br(),
                             "1:  no correlation for this model",
                             br(),
                             "1 - 5: moderate correlation for this model",
                             br(),
                             "> 5: potentially severe correlation for this model"
                           )
                         )
                       ),
                       tabPanel(
                         title = "Solutions", 
                         "What should you do in the case of (multi)collinearity?",
                         br(),
                         br(),
                         "       - If therory suggests that a varible should be present in the model, the variables can be left alone", 
                         br(),
                         br(),
                         "       - One varbiable can be dropped, so long as thoery suggests both are not necessary",
                         br(),
                         br(),
                         "       - Increase sample size"
                       )
                     )
            ),
            tabPanel(title = "Error Variance",
              br(),
              div(HTML("Heteroscadsticity exists when the variance of the error term is <em>not</em> constant, which is a violation of classical assumptions.")),
              br(),
              tabsetPanel(
                tabPanel(title = "Breusch Pagan Test",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("spread_1_select", label = NULL, choices = NULL, selected = NULL, multiple = FALSE),
                      selectInput("spread_2_select", label = NULL, choices = NULL, selected = NULL, multiple = TRUE)
                    ),
                    mainPanel(
                      verbatimTextOutput("spread_output"),
                      br(),
                      "If Prob > Chi2 (p-value) is less than 0.05, the null hypothesis is rejected and heteroskedasticity is present"
                    )
                  )
                ), 
                tabPanel(title = "Solutions",
                  br(),
                  "If heterokedasticity is present in the model, apply White's Robust Standard Error!"
                )
              )
            )
          )
        )
      )
    )
  )
  ))

