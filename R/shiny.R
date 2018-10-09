#' select_cluster
#'
#' @export
select_cluster <- function() {
  runApp(system.file("shiny/app.R", package = "shinycluster"))
}
