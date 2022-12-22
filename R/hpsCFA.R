#'
#' @title Analisis Validitas Konstruk dengan CFA
#' @return Nothing
#' @description Analisis CFA untuk melakukan validitas kontruk.Selain gratis, Package ini diharapkan dapat memberikan kemudahan dibandingkan menggunakan software-sofware berbayar.
#' @details This starts the CFA analysis
#' @keywords CFA
#' @examples
#'  \dontrun{
#' library(shiny)
#' library(readxl)
#' library(psych)
#' library(corrplot)
#' library(lavaan)
#' library(semPlot)
#' library(writexl)
#' hpsCFA()
#' }
#' @export

hpsCFA<- function() {

  shiny::runApp(appDir = system.file("CFAhps", package="hpsCFA"))

}
