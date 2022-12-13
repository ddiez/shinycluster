server_fun <- function(values) {
  server <- function(input, output, session) {
    values <- do.call(reactiveValues, values)

    onStop(
      function() {
        observe({
          stopApp(list(cluster_data = values$cluster_data, cluster = values$cluster, data = values$data))
        })
      }
    )

    observeEvent(input$done, {
      stopApp(list(cluster_data = values$cluster_data, cluster = values$cluster, data = values$data))
    })

    output$ui.xvar <- renderUI({
      cols <- colnames(values$data)
      selectInput("xvar", "xvar", choices = cols, selected = cols[1], width = 150)
    })

    output$ui.yvar <- renderUI({
      cols <- colnames(values$data)
      selectInput("yvar", "yvar", choices = cols, selected = cols[2], width = 150)
    })

    output$ui.color <- renderUI({
      cols <- c("none", "density", "contour", colnames(values$data))
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
      disable("done")
    }

    cluster_finish_editing <- function() {
      values$choose_cluster <- FALSE
      updateActionButton(session, "add", "Add")
      disable("undo")
      disable("clear")
      enable("done")
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

    # output$cluster_info <- renderUI({
    #   ncluster <- values$ncluster
    #
    #   #if (ncluster > 0) {
    #     clusters <- unique(values$cluster)
    #     tl <- lapply(clusters, function(cluster) {
    #       textInput(as.character(cluster), label = cluster, value = cluster, width = "50px")
    #     })
    #     return(tagList(tl))
    #   #}
    # })

    output$download <- downloadHandler(
      filename = "clusters.rds",
      content = function(con) {
        write_rds(list(cluster = values$cluster, data = values$data, cluster_data = values$cluster_data), con)
      }
    )

    output$plot <- renderPlot({
      validate(
        need(input$xvar, ""),
        need(input$yvar, "")
      )

      data <- values$data

      p <- ggplot(data, aes_string(input$xvar, input$yvar), size = input$size)

      if (input$color == "none")
        p <- p + geom_point(color = "grey", size = input$size)

      if (input$color == "density")
        p <- p + geom_bin_2d(bins = 50) + scale_fill_viridis_c(trans = "log10")

      if (input$color == "contour")
        p <- p + geom_point(color = "grey", size = input$size) + geom_density_2d(color = "red")

      if (! input$color %in% c("none", "density")) {
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
        p <- p + geom_path(data = tmp, color = "violetred", lty = "dotted", size = 1)
      }

      if (nrow(tmp) > 0) {
        p <- p + geom_point(data = tmp, size = 3, color = "violetred") +
          geom_point(data = tmp, size = 1, color = "white")
      }

      p + theme_bw(base_size = 14) + guides(color = "none", fill = "none")
    })

    output$plot_info <- renderPlot({
      validate(
        need(input$xvar, ""),
        need(input$yvar, "")
      )

      data <- values$data

      p <- ggplot(data, aes(.data[[input$xvar]], .data[[input$yvar]]))
      p <- p + geom_point(color = "black", alpha = .5, size = input$size) +
        geom_density_2d(color = "red")

      tmp <-  values$cluster_data
      if (nrow(tmp) > 0) {
        p <- p + geom_path(data = tmp, color = "violetred", group = tmp$cluster)
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
      values$data |> mutate(cluster = factor(values$cluster_tmp), final = factor(values$cluster))
    })

    output$debug <- renderPrint({
      values$cluster_data
    })
  }
  server
}
