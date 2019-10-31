
#' Title
#'
#' @param df 
#' @param signal_in 
#' @param corr_file 
#'
#' @return
#' @export
#'
#' @examples
correlation.recalculation <- function(df, signal_in, corr_file){
  corr_data <- read_csv(corr_file, col_names = FALSE, col_types = cols()) %>%
    pull(X1)
  df$value[df$signal == signal_in] <- (corr_data[[1]] + corr_data[[2]] * (df$value[data$signal == signal_in]) + corr_data[[3]] * ((df$value[data$signal == signal_in])^2))
  return(df)
}


#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
read.tecan.xml <- function(file){
  xml.in = xml2::read_xml(file)
  parameters.in <- xml2::xml_find_all(xml.in, "//Parameter")
  parameters <- parse.parameters(parameters.in) 

  parameters.table = tibble::tibble(number = as.character(seq(1, length(parameters))), variable = parameters) %>%
    dplyr::bind_rows(list(number = "Temperature", variable = "Temperature"))
  
  print(parameters.table)
  data.in <- xml2::xml_find_all(xml.in, "//Data")
  data.out <- data.in %>%
      # purrr::map(~parse.cycle(.)) %>%
      furrr::future_map(~parse.cycle(.)) %>%
      dplyr::bind_rows() %>% 
    dplyr::left_join(parameters.table, by = c("variable" = "number")) %>%
    dplyr::select(time, well, variable = variable.y, value)
  
  return(data.out)
}


#' Title
#'
#' @param data.cycle 
#'
#' @return
#' @export
#'
#' @examples
parse.cycle <- function(data.cycle){
  time = convert.time(xml2::xml_attr(data.cycle, 'Time_Start'))
  temperature = as.numeric(xml2::xml_attr(data.cycle, "Temperature"))
  
  temp.df = tibble::tibble(time = time, 
                   well = "Z00", 
                   variable = "Temperature", 
                   value = temperature)
  
  well.df = xml2::xml_find_all(data.cycle, "Well") %>%
    # purrr::map(~parse.well.timepoint(.)) %>%
    furrr::future_map(~parse.well.timepoint(.)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(time = time)
  
  return(dplyr::bind_rows(temp.df, well.df))
}


#' Title
#'
#' @param xml.node 
#'
#' @return
#' @export
#'
#' @examples
parse.well.timepoint <- function(xml.node){
  well = full.well.name(xml2::xml_attr(xml.node, "Pos"))
  
  tmp <- function(input){
    label = xml2::xml_attr(input ,"LabelId")
    value = as.numeric(gsub(",",".", xml2::xml_text(input)))
    return(tibble::tibble(variable = label, value = value))
  }
  
  # extract single measurement data
  data.single = xml2::xml_find_all(xml.node, "Single")  %>%
    # purrr::map(~tmp(.)) %>%
    furrr::future_map(~tmp(.)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(well = full.well.name(well))
  return(data.single)
}

#' Title
#'
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
convert.time <-  function(input){
  days = stringr::str_extract(input, "P.*D") %>%
    gsub("P", "", .) %>%
    gsub("D", "", .) %>%
    as.numeric()
  if (is.na(days)){days = 0}
  hours = stringr::str_extract(input, "T.*H") %>%
    gsub("T", "", .) %>%
    gsub("H", "", .) %>%
    as.numeric()
  if (is.na(hours)){hours = 0}
  
  minutes = stringr::str_extract(input, "H.*M") %>%
    gsub("H", "", .) %>%
    gsub("M", "", .) %>%
    as.numeric()
  if (is.na(minutes)){minutes = 0}
  
  seconds = stringr::str_extract(input, "M.*S") %>%
    gsub("M", "", .) %>%
    gsub("S", "", .) %>%
    as.numeric()
  if (is.na(seconds)){seconds = 0}
  
  return(days * 24 + hours + minutes / 60 + seconds / 3600)
}

#' Title
#'
#' @param well 
#'
#' @return
#' @export
#'
#' @examples
full.well.name <- function(well){
  text = gsub("[[:digit:]]", "", well)
  
  # However, I cannot obtaining the character portion using:
  number = gsub("[[:alpha:]]", "", well)
  number.leading = stringr::str_pad(number, 2, pad = "0")
  
  return(paste0(text, number.leading))
}


#' Title
#'
#' @param parameter_section 
#'
#' @return
#' @export
#'
#' @examples
parse.parameters <- function(parameter_section){
  record_parameter = 0
  par_tibble = NULL
  var_list = list()
  counter = 1
  for (parameter in parameter_section){
    name = xml2::xml_attr(parameter, "Name")
    value = xml2::xml_attr(parameter, "Value")
    unit = xml2::xml_attr(parameter, "Unit")
    
    if (record_parameter == 1){
      par_tibble = dplyr::bind_rows(par_tibble, list(variable = name, value = value) )
    }
    
    if (name == "Mode"){
      if (!is.null(par_tibble)){
        var_list[[counter]] =  par_tibble
        counter = counter + 1 
        
      }
      
      record_parameter = 1
      par_tibble = tibble::tibble(variable = "Type", value = value)
    }
    var_list[[counter]] =  par_tibble
  }
  
  
  
  meas.list <- var_list %>%
    purrr::map_chr(~convert.parameter.df.to.name(.))
  return(meas.list)
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
convert.parameter.df.to.name <- function(df){
  type = df %>% dplyr::filter(variable == "Type") %>% dplyr::pull("value")
  if (type == "Absorbance"){
    wavelength = df %>% dplyr::filter(variable == "Wavelength") %>% dplyr::pull("value")
    return(paste0("ABS_", wavelength, "_", wavelength, "_", "NA"))
  }
  if (type == "Fluorescence Bottom Reading"){
    excitation = df %>% dplyr::filter(variable == "Excitation Wavelength") %>% dplyr::pull("value")
    emission = df %>% dplyr::filter(variable == "Emission Wavelength") %>% dplyr::pull("value")
    gain = df %>% dplyr::filter(variable == "Gain") %>% dplyr::pull("value")
    return(paste0("FLUO_", excitation, "_", emission, "_", gain))
  }
}


