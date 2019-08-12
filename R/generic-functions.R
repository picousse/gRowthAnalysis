


time.to.hour <- function(df, time_col){
  df <- df %>% dplyr::mutate(time_hour = !!sym(time_col)/3600) 
  return(df)
}

