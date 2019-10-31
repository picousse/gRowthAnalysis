
#' Pipe symbol
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of
#' layer_points(ggvis(mtcars, ~mpg, ~wt))
#' # you can write
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
NULL


time.to.hour <- function(df, time_col){
  df <- df %>% dplyr::mutate(time_hour = !!sym(time_col)/3600) 
  return(df)
}

