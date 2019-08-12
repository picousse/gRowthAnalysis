
read.tecan.xml <- function(file_name){
  df = xml2::read_xml(file_name)
  return(df)
}

#' Title
#'
#' @param file_name 
#'
#' @return
#' @export
#'
read.data.csv <- function(file_name){
  return(readr::read_csv(file_name, col_types = cols()))
}

correlation.recalculation <- function(df, signal_in, corr_file){
  corr_data <- read_csv(corr_file, col_names = FALSE, col_types = cols()) %>%
    pull(X1)
  df$value[df$signal == signal_in] <- (corr_data[[1]] + corr_data[[2]] * (df$value[data$signal == signal_in]) + corr_data[[3]] * ((df$value[data$signal == signal_in])^2))
  return(df)
}
