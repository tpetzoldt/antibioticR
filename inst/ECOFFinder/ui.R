library(rhandsontable)

shinyUI(fluidPage(
  headerPanel("Test ECOFFinder"),
  sidebarLayout(
    sidebarPanel(

      h3("Input data"),

      rHandsontableOutput("hot"),

      h3("Start parameters"),

      fluidRow(
      column(3, numericInput("inpMean", label = "Mean", value = NA)),
      column(3, numericInput("inpSd", label = "SD", value = NA)),
      column(3, numericInput("inpK", label = "K", value = NA))
      ),
      checkboxInput("bxAutomatic", "Automatic", TRUE),
      actionButton("runBtn", "Run"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Density",
          plotOutput("density"),
          verbatimTextOutput("summary")
        ),
        tabPanel("Cumulative",
          plotOutput("figure")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel("Info",
          includeHTML("info.html")
        )
      )
    )
  )
))
