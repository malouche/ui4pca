#' launches the Shiny PCA app
#'
#' @export launchAppPCA
#'
#' @return shiny application object
#'
#' @example \dontrun {launchAppPCA()}
#'
#' @importFrom shiny shinyAppUI
#' @importFrom shiny shinyAppServer
#' @import shiny

launchAppPCA <- function(){
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
