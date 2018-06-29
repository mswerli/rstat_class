create_avg_pitch_chart <- function(data, player, values, filter_pitch){
  
  title <- paste(player,' ', filter_pitch)
  
  
  if(values == 'release_speed'){
    
    y_lab <- 'Velocity (MPH)'
    x_lab <- 'Game Date'
  
    data <- data %>% filter(pitch_name == filter_pitch &
                              player_name == player) %>%
      group_by(game_date) %>%
      mutate(min_vel = min(as.numeric(release_speed)),
             max_vel = max(as.numeric(release_speed)),
             avg_vel = mean(as.numeric(release_speed)))
  
    
    data <- data %>% select(game_date, min_vel, max_vel, avg_vel) %>% 
      distinct(.keep_all = TRUE) %>%
      gather(min_vel, max_vel, avg_vel, key = 'def', value = 'vals') %>%
      filter(def == 'avg_vel') 
    
    plot(y = as.numeric(data$vals), x = as.Date(data$game_date), type = 'l',
         ylim = c(round(min(data$vals) - (min(data$vals) * .005)),
                  round(max(data$vals) + (max(data$vals) * .005))),
         xlab = x_lab, ylab = y_lab,
         main = title)
    
    
  }
  
  if(values = 'release_spin_rate'){
    
    y_lab <- 'Spin Rate'
    x_lab <- 'Game Date'
    
    data <- data %>% filter(pitch_name == filter_pitch &
                              player_name == player) %>%
      group_by(game_date) %>%
      mutate(min_spn = min(as.numeric(release_spin_rate)),
             max_spn = max(as.numeric(release_spin_rate)),
             avg_spn = mean(as.numeric(release_spin_rate)))
    
    
    data <- data %>% select(game_date, min_spn, max_spn, avg_spn) %>% 
      distinct(.keep_all = TRUE) %>%
      gather(min_spn, max_spn, avg_spn, key = 'def', value = 'vals') %>%
      filter(def == 'avg_spn')
    
    plot(y = as.numeric(data$vals), x = as.Date(data$game_date), type = 'l',
         ylim = c(round(min(data$vals) - (min(data$vals) * .005)),
                  round(max(data$vals) + (max(data$vals) * .005))),
         xlab = x_lab, ylab = y_lab,
         main = title)
    
  }
  
  
}

create_pitch_scatter <- function(data, player, x, y, filter_pitch = NULL){
  
  if(!is.null(filter_pitch)){
  
    data <- data %>% filter(pitch_name == filter_pitch &
                            player_name == player)
  }else{
    
    data <- data %>% filter(player_name == player)
    
  }

  
  plot(x = data$plate_x, y = data$plate_z, type = 'p')
  
}


compare_groups <- function(data, vals, y, category){
  
  
  xyplot(vals~y,type=c('l','p'),groups= category,data=data,auto.key=T)
  
  
}
