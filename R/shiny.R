#' select_cluster
#'
#' @param x object to be passed to the shiny app.
#' @param ... futher arguments passed down to methods.
#'
#' @export
select_cluster <- function(x, ...) {
  UseMethod("select_cluster")
}

#' @rdname select_cluster
#' @export
select_cluster.data.frame <- function(x, ...) {
  app <- app_fun(x)
  runApp(app)
}

app_fun <- function(x) {
  env_ls <- ls(envir = parent.frame())
  obj_name <- deparse(substitute(x))

  if (! obj_name %in% env_ls)
    stop("Object ", obj_name, " not found.")

  val <- get(obj_name, envir = parent.frame())

  values <- list(
    data = val, # the original dataset.
    choose_cluster = FALSE, # logical; whether we are in new cluster mode.
    ncluster = 0, # current number of clusters.
    cluster_data = data.frame(), # cluster data.
    add_cluster = data.frame(), # data for defining a new cluster.
    cluster_tmp = rep(0, nrow(val)), # temporary cluster membership.
    cluster = rep(0, nrow(val)) # cluster membership.
  )

  server <- server_fun(values)
  ui <- ui_fun()

  shinyApp(ui, server)
}


server_fun <- function(values) {
  server <- function(input, output, session) {
    values <- do.call(reactiveValues, values)

    onStop(
      function() {
        observe({
          stopApp(invisible(list(cluster_data = values$cluster_data, cluster = values$cluster, data = values$data)))
        })
      }
    )

    output$ui.xvar <- renderUI({
      cols <- colnames(values$data)
      selectInput("xvar", "xvar", choices = cols, selected = cols[1], width = 150)
    })

    output$ui.yvar <- renderUI({
      cols <- colnames(values$data)
      selectInput("yvar", "yvar", choices = cols, selected = cols[2], width = 150)
    })

    observeEvent(input$clear, {
      values$add_cluster <- data.frame()
    })

    observeEvent(input$undo, {
      tmp <- values$add_cluster
      if (nrow(tmp) > 0) {
        tmp <- tmp[seq_len(nrow(tmp) - 1), ]
        values$add_cluster <- tmp
      }
    })

    observeEvent(input$add, {
      if (values$choose_cluster) {
        cluster_finish_editing()
      } else {
        cluster_start_editing()
      }
    })

    cluster_start_editing <- function() {
      values$choose_cluster <- TRUE
      updateActionButton(session, "add", "Finish")
      enable("undo")
      enable("clear")
    }

    cluster_finish_editing <- function() {
      values$choose_cluster <- FALSE
      updateActionButton(session, "add", "Add")
      disable("undo")
      disable("clear")
    }

    cluster_finish <- function() {
      values$add_cluster <- bind_rows(values$add_cluster, values$add_cluster[1, ])
      values$cluster_data <- bind_rows(values$cluster_data, values$add_cluster)
      values$add_cluster <- data.frame()
      values$ncluster <- values$ncluster + 1
      assign_cluster()
    }

    cluster_add_point <- function(x, y) {
      tmp <- list()
      tmp[[input$xvar]] <- x
      tmp[[input$yvar]] <- y
      tmp[["cluster"]] <- values$ncluster + 1
      tmp <- as.data.frame(tmp)

      values$add_cluster <- bind_rows(values$add_cluster, tmp)
    }

    assign_cluster <- function() {
      cluster_data <- values$cluster_data
      cluster <- values$ncluster
      data <- values$data

      pol.x <- cluster_data[[input$xvar]][cluster_data$cluster == cluster]
      pol.y <- cluster_data[[input$yvar]][cluster_data$cluster == cluster]
      sel <- point.in.polygon(data[[input$xvar]], data[[input$yvar]], pol.x, pol.y) == 1

      values$cluster[sel] <- cluster
    }

    observeEvent(input$plot_dblclick, {
      if (values$choose_cluster) {
        cluster_add_point(input$plot_dblclick$x, input$plot_dblclick$y)
        cluster_finish()
      }
    })

    observeEvent(input$plot_click, {
      if (values$choose_cluster) {
        cluster_add_point(input$plot_click$x, input$plot_click$y)
      }
    })

    output$plot <- renderPlot({
      validate(
        need(input$xvar, ""),
        need(input$yvar, "")
      )

      data <- values$data %>% mutate(cluster = values$cluster_tmp)

      tmp <- bind_rows(data, values$cluster_data, values$add_cluster)
      tmp <- tmp %>% mutate(cluster = factor(.data[["cluster"]]))
      tmp2 <- tmp %>% filter(.data[["cluster"]] != "0")

      ncluster <- values$ncluster + 1

      p <- ggplot(tmp, aes_string(input$xvar, input$yvar, color = "cluster", size = "cluster")) +
        geom_point() +
        scale_size_manual(values = c(1, rep(3, ncluster)))

      if (nrow(tmp2) > 1) {
        p <- p + geom_path(data = tmp2, size = 1)
      }

      p + geom_point(data = tmp2, size = 2, color = "white")
    })

    output$ncluster <- renderPrint({
      values$ncluster
    })

    output$info <- renderPrint({
      values$add_cluster
    })

    output$table <- renderDataTable({
      values$data %>% mutate(cluster = factor(values$cluster_tmp), final = factor(values$cluster))
    })

    output$debug <- renderPrint({
      values$cluster_data
    })
  }
  server
}

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
        hr(),
        h3("Debug info"),
        verbatimTextOutput("ncluster"),
        verbatimTextOutput("info"),
        verbatimTextOutput("debug")
      ),

      mainPanel(
        plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick"),
        dataTableOutput("table")
      )
    )
  )
  ui
}
