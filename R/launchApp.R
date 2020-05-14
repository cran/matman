#' Launches a demo app
#'
#' Launches a shiny app that demonstrates how to use the functions provides by package matman.
#'
#' @author Leon Binder \email{leon.binder@@th-deg.de}
#' @author Bernhard Bauer \email{bernhard.bauer@@th-deg.de}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @example \dontrun {launchApp()}
#'
#' @export
matmanDemo <- function() {
  shinyApp(ui = .shinyAppUI, server = .shinyAppServer)
}

