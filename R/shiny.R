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
  env_ls <- ls(envir = parent.frame())
  obj_name <- deparse(substitute(x))

  val <- get(obj_name, envir = parent.frame())

  values <- list(
    data = val, # the original dataset.
    choose_cluster = FALSE, # logical; whether we are in new cluster mode.
    ncluster = 0L, # current number of clusters.
    cluster_data = data.frame(), # cluster data.
    add_cluster = data.frame(), # data for defining a new cluster.
    cluster_tmp = rep(0L, nrow(val)), # temporary cluster membership.
    cluster = rep(0L, nrow(val)) # cluster membership.
  )

  app <- app_fun(values)
  runApp(app)
}

#' @rdname select_cluster
#' @export
select_cluster.list <- function(x, ...) {
  values <- list(
    data = x$data, # the original dataset.
    choose_cluster = FALSE, # logical; whether we are in new cluster mode.
    ncluster = as.integer(length(x$cluster)), # current number of clusters.
    cluster_data = x$cluster_data, # cluster data.
    add_cluster = data.frame(), # data for defining a new cluster.
    cluster_tmp = x$cluster, # temporary cluster membership.
    cluster = x$cluster # cluster membership.
  )

  app <- app_fun(values)
  runApp(app)
}

app_fun <- function(x) {


  server <- server_fun(x)
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

    output$ui.color <- renderUI({
      cols <- c("none", colnames(values$data))
      selectInput("color", "color", choices = cols, selected = 1, width = 150)
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
      values$ncluster <- values$ncluster + 1L
      assign_cluster()
    }

    cluster_add_point <- function(x, y) {
      tmp <- list()
      tmp[[input$xvar]] <- x
      tmp[[input$yvar]] <- y
      tmp[["cluster"]] <- values$ncluster + 1L
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

      data <- values$data

      p <- ggplot(data, aes_string(input$xvar, input$yvar), size = input$size)

      if (input$color == "none")
        p <- p + geom_point(color = "grey", size = input$size)
      else {
        cl <- class(data[[input$color]])
        if (cl %in% c("numeric", "integer")) {
          p <- p + geom_point(aes_string(color = input$color), size = input$size) +
            scale_color_viridis_c()
        }
        if (cl %in% c("character", "factor")) {
          p <- p + geom_point(aes_string(color = input$color), size = input$size) +
            scale_color_brewer(palette = "Set1")
        }
      }

      tmp <-  values$cluster_data
      if (nrow(tmp) > 0) {
        p <- p + geom_path(data = tmp, color = "violetred", group = tmp$cluster)
      }

      tmp <-  values$add_cluster
      if (nrow(tmp) > 1) {
        p <- p + geom_path(data = tmp, color = "black", lty = "dotted")
      }

      if (nrow(tmp) > 0) {
          p <- p + geom_point(data = tmp, size = 3, color = "black") +
            geom_point(data = tmp, size = 1, color = "white")
      }

      p + theme_bw(base_size = 14)
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
        uiOutput("ui.color"),
        sliderInput("size", "size", min = .1, max = 3, value = 1, step = .1, ticks = FALSE, width = 200),
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
