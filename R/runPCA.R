#' @title A Shiny App for PCA
#'
#' @description This package contains a shiny app that helps to perform a PCA without any coding and where you can download every obtained results: figures, tables...
#'
#' @param void
#'
#' @return run a Shiny app
#'
#' @examples runPCA()
#'
#' @export

runPCA<-function(){
  library(shiny)
  runApp("R/")
}
