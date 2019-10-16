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



# library(tidyverse)
# library(furrr)
# plan(multiprocess)
# 
# 
# # out variable
# out = tibble(time = double(),
#              well = character(),
#              label = integer(),
#              value = double())
# 
# out.list = list()
# counter = 1 
# 
# # 
# # # read the xml
# xml.in = read_xml("data/20190618_bidir_GFPmorange_GFPgain95/2019-06-07 15-56-57_plate_1.xml")
# # extract the data part.
# data.in <- xml_find_all(xml.in, "//Data")
# 
# test <- data.in %>%
#   future_map(~parse.cycle(.)) %>%
#   bind_rows()
# 
# s = read.xml("data/20190618_bidir_GFPmorange_GFPgain95/2019-06-07 15-56-57_plate_1.xml")
# 
# file = "data/20190618_bidir_GFPmorange_GFPgain95/2019-06-07 15-56-57_plate_1.xml"

read.xml <- function(file){
  xml.in = read_xml(file)
  parameters.in <- xml_find_all(xml.in, "//Parameter")
  parameters <- parse.parameters(parameters.in) 

  parameters.table = tibble(number = as.character(seq(1, length(parameters))), variable = parameters) %>%
    bind_rows(list(number = "Temperature", variable = "Temperature"))
  
  print(parameters.table)
  data.in <- xml_find_all(xml.in, "//Data")
  data.out <- data.in %>%
      future_map(~parse.cycle(.)) %>%
      bind_rows() %>% 
    left_join(parameters.table, by = c("variable" = "number")) %>%
    select(time, well, variable = variable.y, value)
  
  return(data.out)
}


parse.cycle <- function(data.cycle){
  time = convert.time(xml_attr(data.cycle, 'Time_Start'))
  temperature = as.numeric(xml_attr(data.cycle, "Temperature"))
  
  temp.df = tibble(time = time, 
                   well = "Z00", 
                   variable = "Temperature", 
                   value = temperature)
  
  well.df = xml_find_all(data.cycle, "Well") %>%
    future_map(~parse.well.timepoint(.)) %>%
    bind_rows() %>%
    mutate(time = time)
  
  return(bind_rows(temp.df, well.df))
}


parse.well.timepoint <- function(xml.node){
  well = full.well.name(xml_attr(xml.node, "Pos"))
  
  tmp <- function(input){
    label = xml_attr(input ,"LabelId")
    value = as.numeric(gsub(",",".", xml_text(input)))
    return(tibble(variable = label, value = value))
  }
  
  # extract single measurement data
  data.single = xml_find_all(xml.node, "Single")  %>%
    map(~tmp(.)) %>%
    bind_rows() %>%
    mutate(well = full.well.name(well))
  return(data.single)
}

convert.time <-  function(input){
  days = str_extract(input, "P.*D") %>%
    gsub("P", "", .) %>%
    gsub("D", "", .) %>%
    as.numeric()
  if (is.na(days)){days = 0}
  hours = str_extract(input, "T.*H") %>%
    gsub("T", "", .) %>%
    gsub("H", "", .) %>%
    as.numeric()
  if (is.na(hours)){hours = 0}
  
  minutes = str_extract(input, "H.*M") %>%
    gsub("H", "", .) %>%
    gsub("M", "", .) %>%
    as.numeric()
  if (is.na(minutes)){minutes = 0}
  
  seconds = str_extract(input, "M.*S") %>%
    gsub("M", "", .) %>%
    gsub("S", "", .) %>%
    as.numeric()
  if (is.na(seconds)){seconds = 0}
  
  return(days * 24 + hours + minutes / 60 + seconds / 3600)
}

full.well.name <- function(well){
  text = gsub("[[:digit:]]", "", well)
  
  # However, I cannot obtaining the character portion using:
  number = gsub("[[:alpha:]]", "", well)
  number.leading = str_pad(number, 2, pad = "0")
  
  return(paste0(text, number.leading))
}


parse.parameters <- function(parameter_section){
  record_parameter = 0
  par_tibble = NULL
  var_list = list()
  counter = 1
  for (parameter in parameter_section){
    name = xml_attr(parameter, "Name")
    value = xml_attr(parameter, "Value")
    unit = xml_attr(parameter, "Unit")
    
    if (record_parameter == 1){
      par_tibble = bind_rows(par_tibble, list(variable = name, value = value) )
    }
    
    if (name == "Mode"){
      if (!is.null(par_tibble)){
        var_list[[counter]] =  par_tibble
        counter = counter + 1 
        
      }
      
      record_parameter = 1
      par_tibble = tibble(variable = "Type", value = value)
    }
    
  }
  
  var_list[[counter]] =  par_tibble
  
  meas.list <- var_list %>%
    map_chr(~convert.to.name(.))
  return(meas.list)
}

convert.to.name <- function(df){
  type = df %>% filter(variable == "Type") %>% pull("value")
  if (type == "Absorbance"){
    wavelength = df %>% filter(variable == "Wavelength") %>% pull("value")
    return(paste0("ABS_", wavelength, "_", wavelength, "_", "NA"))
  }
  if (type == "Fluorescence Bottom Reading"){
    excitation = df %>% filter(variable == "Excitation Wavelength") %>% pull("value")
    emission = df %>% filter(variable == "Emission Wavelength") %>% pull("value")
    gain = df %>% filter(variable == "Gain") %>% pull("value")
    return(paste0("FLUO_", excitation, "_", emission, "_", gain))
  }
}


