general.plotting <- function(df, col){
  # create unique variable beforehand, as well as filtering.
  p = ggplot(df, aes(x=time_to_hour, y = value, color = strain, group=well)) + 
    geom_point() + 
    geom_line() + 
    facet_grid(signal ~ . , scales = "free")

  return(p)
}