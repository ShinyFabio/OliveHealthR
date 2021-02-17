#' Select column from a dataframe
#' 
#' @description Save the name of a column dataframe using an input from the UI. Can be for example from a \code{selectInput()}.
#'
#' @import shiny
#' 
#' @param data The dataset used to select the column. Should be a reactive value (e.g. datmap1())
#' @param input the input from the ui (e.g. the input from a \code{selectInput()}) in the form of \code{input$...}.
#' 
#' @examples \dontrun{
#' 
#'   showcolumn2 = reactive({
#'   
#'   Olv_select_col(
#'   data = datmap1(),       #a dataset made with read_csv() 
#'   input = input$select3   #the input from a shiny::selectInput()
#'   )
#'   
#'   })
#' 
#' }
#'




Olv_select_col = function(data, input){
    req(data)
    if (is.null(input) || input == "")
      return(data)
    else
      data[, colnames(data) %in% input]
}
