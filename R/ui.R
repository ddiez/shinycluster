ui_fun <- function() {
  ui <- fluidPage(
    useShinyjs(),
    titlePanel("Select clusters"),

    sidebarLayout(
      sidebarPanel(
        actionButton("add", "Add", width = 100),
        disabled(actionButton("undo", "Undo", width = 100)),
        disabled(actionButton("clear", "Clear", width = 100)),
        hr(),
        uiOutput("ui.xvar"),
        uiOutput("ui.yvar"),
        uiOutput("ui.color"),
        sliderInput("size", "size", min = .1, max = 3, value = 1, step = .1, ticks = FALSE, width = 200),
        hr(),
        downloadButton("download", "Download"),
        hr(),
        h3("Debug info"),
        verbatimTextOutput("ncluster"),
        verbatimTextOutput("info"),
        verbatimTextOutput("debug")
      ),

      mainPanel(
        plotOutput("plot", width = "400px", click = "plot_click", dblclick = "plot_dblclick"),
        dataTableOutput("table")
      )
    )
  )
  ui
}