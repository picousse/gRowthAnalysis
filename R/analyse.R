
# data clean up -----------------------------------------------------------


#' calculate the exponential time of the curve. This is done by fitting a spline, and calculating when the first derivative goes below 0.
#'
#' @param df (dataframe). Dataframe containing the data
#' @param input_time (string). Column name that contains the time value (in hours!). (Default = "time")
#' @param input_value (string). Column name that contains the measured value. (Default = "value")
#' @param treshold (double). End of exponential time is only calculated if this treshold has been surpassed. This mainly filters out the noise often seen in the beginnen of the curce.
#' @param lamda (double), Smoothing paraneter. this needed to filter out outliers.
#'
#' @return double. End of the exponential phase
#' @export
#' 
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
#' 
calc.exp.time <- function(df = NULL, input_time = "time", input_value = "value",  treshold = 0.3, lambda = 1e-5){
  if (is.null(df)){stop("There is no dataframe given.")}

  df <- df %>% filter( !!(sym(input_value)) > !!treshold)
  
  fit = smooth.spline(df[[input_time]], df[[input_value]], cv=TRUE, lambda = lambda)

  fit.pred.deriv1 = predict(fit, deriv = 1)
  
  # calculate running average.
  time.average = moving.average(fit.pred.deriv1$x, n= 5 )
  value.average = moving.average(fit.pred.deriv1$y, n= 5 )
  plot(time.average,value.average)
  # abline(h=0)
  
  # calculate absolute values
  value.average.abs = abs(value.average)
  index = which.min(value.average.abs)
  
  exp.time = time.average[index]
  
  return(exp.time)
}


#' calculate the moving average of a dataseries
#'
#' @param x (vector). Vector containing the data that for which the average needs to be calculated.
#' @param n (integer). Number of values for which the average needs to be calculated.
#'
#' @return vector. Data vector containing the averages. This vector is as long as the input (and thus contains NA's)
#' @export
#' 
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
moving.average <- function(x, n = 5){
  tmp = stats::filter(x, rep(1 / n, n), sides = 2)
  tmp = as.vector(tmp)
  tmp = na.omit(tmp)
  tmp = as.vector(tmp)
  return(tmp)
}



# q calc ------------------------------------------------------------------

#' Calculate mu, based on a certain model
#'
#' @param df (dataframe). dataframe containing all necessary data for the calculation
#' @param model (string). model to is used to fit. (default: "gompertz")
#'
#' @return grofit object
#' @export
#'
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
q.calc.individual <- function(df, model = "gompertz"){

  data <- df %>%
    dplyr::mutate(value.log = purrr::map_dbl(value, ~log(. / min(value[1:20]))))
  
  fit = tryCatch({
    grofit::gcFitModel(data$time, 
                      data$value.log, 
                      gcID="undefined", 
                      control=grofit::grofit.control(suppress.messages=TRUE,model.type = c(model)))
  }, warning = function(w) {
    return(NA)
  }, error = function(e) {
    return(NA)
  }, finally = {
  })
  
  
  
  
  
  
  
  
  # 
  # fit <- grofit::gcFitModel(data$time, 
  #                          data$value.log, 
  #                          gcID="undefined", 
  #                          control=grofit::grofit.control(suppress.messages=TRUE,model.type = c(model)))
  
  # if (fit$fitFlag){
  #   plot <- ggplot2::ggplot() +
  #     ggplot2::geom_point(aes (x = fit$raw.time, y = fit$raw.data)) +
  #     ggplot2::geom_point(aes (x = data$time_hour, y=data$value), color = "blue", size = "1") +
  #     ggplot2::geom_line(aes (x = fit$fit.time , y = fit$fit.data), color = "red") +
  #     ggplot2::geom_abline(intercept = -fit$parameters$mu * fit$parameters$lambda, slope = fit$parameters$mu ) +
  #     ggplot2::scale_x_continuous(minor_breaks = waiver()) +
  #     ggplot2::scale_y_continuous(minor_breaks = waiver()) +
  #     ggplot2::ggtitle(paste(name, "Fitted data ", calc.signal)) +
  #     ggplot2::theme(panel.grid.major = element_line(colour = "gray80"))
  # }else{
  #   
  # }
  
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

#' plot the q fit
#'
#' @param fit 
#' @param calc.signal 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
q.plot.individual <- function(fit = NULL, calc.signal = NULL , name = NULL){
  if (is.null(df)){stop("There is no dataframe given.")}
  if (is.null(calc.signal)){stop("There is no signal given.")}
  if (is.null(name)){stop("There is no name given.")}
  
  calc.signal = enquo(calc.signal)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(aes (x = fit$raw.time, y = fit$raw.data)) +
    ggplot2::geom_line(aes (x = fit$fit.time , y = fit$fit.data), color = "red") +
    ggplot2::geom_abline(intercept = -fit$parameters$mu * fit$parameters$lambda, slope = fit$parameters$mu ) +
    ggplot2::scale_x_continuous(minor_breaks = waiver()) +
    ggplot2::scale_y_continuous(minor_breaks = waiver()) +
    ggplot2::ggtitle(paste(name, "Fitted data ", calc.signal)) +
    ggplot2::theme(panel.grid.major = element_line(colour = "gray80"))
  
  return(p)
  
}

#' Title
#'
#' @param df 
#' @param time 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
q.plot.raw <- function(df, time = "time", value = "value"){
  
  p <- ggplot2::ggplot(df, aes(x = time , y = value)) + 
    ggplot2::geom_point()
  
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
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
q.extract.data.individual <- function(grofit.model){
  
  
  out <- list(mu = NA,
                A = NA,
                lambda = NA,
                integral = NA)
  
  
  if ("fitFlag" %in% names(grofit.model)){
    if(grofit.model$fitFlag){
    out$mu = grofit.model$parameters$mu[['Estimate']]
    out$A = grofit.model$parameters$A[['Estimate']]
    out$lambda = grofit.model$parameters$lambda[['Estimate']]
    out$integral = grofit.model$parameters$integral
    }
  }
  
  
  # df_out <- df %>%
  #   mutate(fit.flag = map_lgl(!!col_var, ~.$fitFlag)) %>%
  #   mutate(mu = map_dbl(!!col_var, ~ifelse(class(.$parameters$mu) == "logical", NA, .$parameters$mu[['Estimate']] ))) %>%
  #   mutate(A = map_dbl(!!col_var, ~ifelse(class(.$parameters$A) == "logical", NA, .$parameters$A[['Estimate']] ))) %>%
  #   mutate(lambda = map_dbl(!!col_var, ~ifelse(class(.$parameters$lambda) == "logical", NA, .$parameters$lambda[['Estimate']] ))) %>%
  #   mutate(integral = map_dbl(!!col_var, ~ifelse(class(.$parameters$integral) == "logical", NA, .$parameters$integral ))) %>%
  #   select(well, fit.flag, mu, A, lambda, integral)
  return(out)
}



#' Title
#'
#' @param df 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
q.extract.data <- function(df, col){
  
  out = df %>%
    mutate(tmp = map( !!(sym(col)), ~q.extract.data.individual(.))) %>%
    unnest_wider(tmp)
  
  return(out)
}


# q.calc.df <-function(df = NULL, calc.signal = NULL, data.col = NULL, model = "gompertz"){
#   if (is.null(df)){stop("There is no dataframe given.")}
#   if (is.null(calc.signal)){stop("There is no signal given.")}
#   if (is.null(data.col)){stop("There data col given.")}
#   
#   # create dataframe with fit model, extract data and plots
#   
#   tmp <- df %>% 
#     mutate(growth.parameters = map(data.col, ~calc.q(df = .,calc.signal = calc.signal ))) %>%
#     mutate(raw_data = map)
#   
#   
#   # use calc.q.individual
#   # plot q
# }
# 




# calc ratio --------------------------------------------------------------




#' calcuate ratio q
#'
#' @param df (datafrane). Dataframe containing the data
#' @param x.signal (string). column name to be used in the x.axis
#' @param y.signal (string). column name to be used in the y.axis
#'
#' @return fitobject
#' @export
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
ratio.calc.individual <- function(df, x.signal = NULL, y.signal = NULL){
  if (is.null(x.signal)){stop("There is no x.sginal given.")}
  if (is.null(y.signal)){stop("There is no y.sginal given.")}
  
  df.tmp <- df %>%
    select( X = !!x.signal, Y = !!y.signal)
  
  fit = lm(df.tmp$Y ~ df.tmp$X )
    
  return(fit)
  
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
#' @author Pieter Coussement, \email{coussementpieter@@gmail.com}
ratio.extract.data <- function(df, col){
  col_var = enquo(col)
  
  df_out <- df %>%
    mutate(ratio.B = map_dbl(!!col_var, ~.$coefficients[[1]])) %>%
    mutate(ratio.A = map_dbl(!!col_var, ~.$coefficients[[2]])) %>%
    select(well, ratio.A, ratio.B)
  
  return(df_out)
}




