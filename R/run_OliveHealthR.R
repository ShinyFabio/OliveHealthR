#' Launches the web shiny App "OliveHealthR"
#'
#' @description Simple function to connect UI and Server of the shiny App and launch it inside the default browser.
#'
#' @usage run_OliveHealthR()
#' 
#' @note OliveHealthR is builded with {golem}.
#' 
#' @examples 
#' \dontrun{
#' libraray(OliveHealthR)
#' run_OliveHealthR()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_OliveHealthR <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
