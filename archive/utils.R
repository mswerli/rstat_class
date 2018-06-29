get_data <- function(parms, url = readLines('base-url.txt')){
  
  for(a in names(parms)){
    
    url <- param_set(url, a, parms[[a]])
  }
  
  params <- param_get(url, c('team','hfSea', 'player_type'))
  
  file_name <- sprintf('data/%s-%s-%s', 
                       params$team, params$hfSea, params$player_type)
  
  data <- download.file(url,  file_name)
  
  df <- read.csv(file_name)
  
  return(df)
  
  
}

clean_data <- function(data){
  for(a in names(data)){
    data[,a][data[,a] == 'null' | data[,a] == ''] <- NA
  }
  
  dep_index <- which(!str_detect(names(data), '_deprecated'))
  
  data <- data[,dep_index]
  
  return(data)
}


