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
    cluster_data = data.frame(), # cluster data.
    cluster = rep(0L, nrow(val)), # cluster membership.
    data = val # the original dataset.
  )

  select_cluster(values)
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

  runApp(app_fun(values))
}

app_fun <- function(x) {
  server <- server_fun(x)
  ui <- ui_fun()

  shinyApp(ui, server)
}
