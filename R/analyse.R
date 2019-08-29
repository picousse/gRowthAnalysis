#' Title
#'
#' @param df 
#' @param signal 
#' @param input_time 
#' @param treshold 
#'
#' @return
#' @export
#'
#' @examples
calc.exp.time <- function(df = NULL, signal = NULL, input_time = "time_hour", treshold = 0.3){
  if (is.null(df)){stop("There is no dataframe given.")}
  if (is.null(signal)){stop("There is no signal given.")}
  
  df_tmp <- df %>% 
    filter(signal == !!(signal)) %>%
    filter(value > !!(treshold))
  
  #calculation of the diff time, needed for plotting
  diff_time = c()
  for (t in 1:length(df_tmp[[input_time]])-1){
    diff_time[t]=mean(c(df_tmp[[input_time]][t+1], df_tmp[[input_time]][t]))
  }
  
  #calculation of the first prime
  value.prime <- diff(df_tmp$value)/diff(df_tmp[[input_time]])
  
  #Different approach. Apply a smoothing line through the differential. When this lines crosses the X-axis at y= 0, that is where the exp stops.
  spl2 <- smooth.spline(x=diff_time, y=value.prime)
  pred.prime <- predict(spl2)
  
  #look for the maximum first. In other words look where the curve is no longer increasing for 3 data points. From that point on, check when the curve crosses at y=0
  for (t in 1:(length(pred.prime$x)-1)){
    if (pred.prime$y[t]>0 && pred.prime$y[t+1]<=0 && df_tmp$value[t]>treshold){
      final_time=pred.prime$x[t]
      break
    }else{
      final_time = tail(df_tmp$time_hour, n=1)
    }
  }
  
  return(final_time)
}

#' Calculate mu, based on a certain model
#'
#' @param df 
#' @param calc.signal 
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
calc.q <- function(df = NULL, calc.signal = NULL, model = "gompertz"){
  if (is.null(df)){stop("There is no dataframe given.")}
  if (is.null(signal)){stop("There is no signal given.")}
  # print(calc.signal)
  data <- df %>%
    dplyr::filter(signal == calc.signal )
  
  # print(data)
  
  data = data%>%
    dplyr::mutate(value.log = furrr::future_map_dbl(value, ~log(. / min(value[1:20]))))
    
  fit = grofit::gcFitModel(data$time_hour, 
                       data$value.log, 
                       gcID="undefinded", 
                       control=grofit::grofit.control(suppress.messages=TRUE,model.type = c(model)))
    

  #   dplyr::mutate(raw_plot = furrr::future_map(.x = data,  #single plot? (why 2?)
  #                                              ~ggplot2::ggplot(.x, aes(x = delta_time, y = variable)) +
  #                                                ggplot2::geom_point() +
  #                                                ggplot2::ggtitle(paste(name, "raw_data", parameter)) +
  #                                                ggplot2::theme(panel.grid.major = element_line(colour = "gray80")))) %>%
  #   dplyr::mutate(fit_plot = furrr::future_map(.x = fit,
  #                                              ~ggplot2::ggplot() +
  #                                                ggplot2::geom_point(aes (x = .x$raw.time, y = .x$raw.data)) +
  #                                                ggplot2::geom_line(aes (x = .x$fit.time , y = .x$fit.data), color = "red") +
  #                                                ggplot2::geom_abline(intercept = -.x$parameters$mu * .x$parameters$lambda, slope = .x$parameters$mu ) +
  #                                                ggplot2::scale_x_continuous(minor_breaks = waiver()) +
  #                                                ggplot2::scale_y_continuous(minor_breaks = waiver()) +
  #                                                ggplot2::ggtitle(paste(name, "Fitted data ", parameter)) +
  #                                                ggplot2::theme(panel.grid.major = element_line(colour = "gray80"))))
  # 
  # out <- data_fit[c("name", "mu", "A", "max", "integral", "lambda")] 
  
  
  return(fit)
}

#' Extract calculated growth parameters into a dataframe
#'
#' @param df 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
extract.q.data <- function(df, col){
  col_var = enquo(col)

  df_out <- df %>%
    mutate(fit.flag = map_lgl(!!col_var, ~.$fitFlag)) %>%
    mutate(mu = map_dbl(!!col_var, ~ifelse(class(.$parameters$mu) == "logical", NA, .$parameters$mu[['Estimate']] ))) %>%
    mutate(A = map_dbl(!!col_var, ~ifelse(class(.$parameters$A) == "logical", NA, .$parameters$A[['Estimate']] ))) %>%
    mutate(lambda = map_dbl(!!col_var, ~ifelse(class(.$parameters$lambda) == "logical", NA, .$parameters$lambda[['Estimate']] ))) %>%
    mutate(integral = map_dbl(!!col_var, ~ifelse(class(.$parameters$integral) == "logical", NA, .$parameters$integral ))) %>%
    select(well, fit.flag, mu, A, lambda, integral)
  return(df_out)
}

#' calcuate ratio q
#'
#' @param df 
#' @param x.signal 
#' @param y.signal 
#'
#' @return
#' @export
#'
#' @examples
calc.ratio <- function(df = NULL, x.signal = NULL, y.signal = NULL){
  
  df.tmp <- df %>%
    filter(signal == x.signal | signal == y.signal) %>%
    spread(signal, value) 
  
  fit = lm(df.tmp[[y.signal]] ~ df.tmp[[x.signal]] )
    
  return(fit)
  

  #   #different fittings for all signals
  # smallDataset$signal= factor(smallDataset$signal)
  # signals= levels(smallDataset$signal)
  
  #extract ABS from the signals list
  
    #extracting the data
    # Xval = dfOdFluo[dfOdFluo$signal == "ABS_600_600_NA",]
    # Yval = dfOdFluo[dfOdFluo$signal == sig ,]
    # 
    # #do the fitting      
    # lineair.fitting = lm(Yval$value ~ Xval$value)
    # 
    # #collecting the data
    # dataOutput = data.frame(strain = strain, 
    #                         signal =  paste(as.character(sig), "vsOD", sep=""),
    #                         well = well, 
    #                         medium = medium,
    #                         parameter = "FluoVsOD",
    #                         value = lineair.fitting$coefficients[[2]],
    #                         stringsAsFactors=FALSE
    # )
    # 
    # #plotting
    # # filename_plot =paste("./Figures/",strain,"/",well,"_",sig, "vsOD",".jpg", sep="")
    # #jpeg(filename= filename_plot) 
    # fluorescenceVsOD = ggplot() +
    #   geom_point(aes(x=Xval$value, y=Yval$value), color="red", size = 4) +
    #   geom_point(aes(x=smallDataset[smallDataset$signal == "ABS_600_600_NA","value"], y=smallDataset[smallDataset$signal == sig, "value"])) +
    #   ggtitle(paste(strain, medium, well, sep=" "))
    # #print(fluorescenceVsOD)
    # #dev.off()
    # 
    # 
    # #ggsave(paste("./Figures/",strain,"/",well,"_",sig, "vsOD",".png", sep=""), width=15, height=10, units="cm")
    # 
    # 
    # 
    # 
    # 
    # #adding the data
    # out = rbind( dataOutput, out)
}


#' extract ratio data
#'
#' @param df 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
extract.ratio.data <- function(df, col){
  col_var = enquo(col)
  
  df_out <- df %>%
    mutate(ratio.B = map_dbl(!!col_var, ~.$coefficients[[1]])) %>%
    mutate(ratio.A = map_dbl(!!col_var, ~.$coefficients[[2]])) 
  
  return(df_out)
}




