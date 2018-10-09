library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)
library(sp)

x <- prcomp(iris[, -5])
y <- data.frame(x$x[, 1:2], Species = iris[, 5])
y <- y %>% rename(x = PC1, y = PC2) %>% mutate(cluster = 0, final = 0)

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Select clusters"),

    sidebarLayout(
        sidebarPanel(
            actionButton("add", "Add clusters", width = 100),
            disabled(actionButton("clear", "Clear")),
            h3("# clusters"),
            verbatimTextOutput("ncluster"),
            br(),
            verbatimTextOutput("info")
        ),

        mainPanel(
           plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick"),
           dataTableOutput("table")
        )
    )
)

server <- function(input, output, session) {
    onStop(
        function() {
            observe({
                stopApp(list(cluster_data = values$cluster_data, data = values$data))
            })
        }
    )

    values <- reactiveValues(
        data = y,
        choose_cluster = FALSE,
        ncluster = 0,
        cluster_data = data.frame(),
        add_cluster = data.frame()
    )

    observeEvent(input$clear, {
        values$add_cluster = data.frame()
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
        enable("clear")
    }

    cluster_finish_editing <- function() {
        values$choose_cluster <- FALSE
        updateActionButton(session, "add", "Add clusters")
        disable("clear")
    }

    cluster_finish <- function() {
        values$add_cluster <- bind_rows(values$add_cluster, values$add_cluster[1, ])
        values$cluster_data <- bind_rows(values$cluster_data, values$add_cluster)
        values$add_cluster <- NULL
        values$ncluster <- values$ncluster + 1
        assign_cluster()
    }

    cluster_add_point <- function(x, y) {
        tmp <- data.frame(
            x = x,
            y = y,
            cluster = values$ncluster + 1)
        values$add_cluster <- bind_rows(values$add_cluster, tmp)
    }

    assign_cluster <- function() {
        cluster_data <- values$cluster_data
        cluster <- values$ncluster

        pol.x <- cluster_data$x[cluster_data$cluster == cluster]
        pol.y <- cluster_data$y[cluster_data$cluster == cluster]
        sel <- point.in.polygon(y$x, y$y, pol.x, pol.y) == 1
        values$data$final[sel] <<- cluster
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
        tmp <- bind_rows(y, values$cluster_data, values$add_cluster)
        tmp <- tmp %>% mutate(cluster = factor(cluster))
        tmp2 <- tmp %>% filter(cluster != "0")

        ncluster <- values$ncluster + 1

        p <- ggplot(tmp, aes(x, y, color = cluster, size = cluster)) +
            geom_point() +
            scale_size_manual(values = c(1, rep(3, ncluster))) +
            theme(aspect.ratio = 1)

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
        values$data
    })
}

shinyApp(ui = ui, server = server)
