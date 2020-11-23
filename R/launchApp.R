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
  warning("matmanDemo currently only works if you install matman by source - 
  otherwise there will be the error 'Couldn't normalize path in `addResourcePath`': \n
  use:  install.packages('matman', type='source') \n
  Do you want to proceed? [y]/n", immediate.=TRUE)
  
  user_input = readline()
  
  cat(user_input)
  
  if (user_input == "y" | user_input == "") {
    shinyApp(ui = .shinyAppUI, server = .shinyAppServer)
  }
}

