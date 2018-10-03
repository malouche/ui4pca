#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny,ggplot2, plyr, FactoMineR, factoextra, DT, ggrepel, scales,grid,gridExtra
#'

launchApp <- function(){
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
