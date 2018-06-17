stat_cast <- R6::R6Class('stat_cast_data',
            private = list(
              base_url = readLines('base-url.txt'),
              pitch_fields =  c('pitch_type', 'game_date', 'release_speed', 'release_pos_x', 
                                'release_pos_z', 'player_name', 'batter', 'pitcher', 'description', 
                                'zone', 'p_throws', 'home_team', 'away_team','type', 'hit_location', 
                                'bb_type', 'balls', 'strikes', 'pfx_x', 'pfx_z', 'plate_x', 'plate_z', 
                                'on_3b', 'on_2b', 'on_1b', 'outs_when_up', 'hc_x', 'hc_y', 'vx0', 'vy0', 
                                'vz0', 'ax', 'ay', 'az', 'sz_top', 'sz_bot', 'hit_distance_sc', 'launch_speed', 
                                'launch_angle', 'release_spin_rate', 'release_extension', 'release_pos_y', 
                                'estimated_ba_using_speedangle', 'estimated_woba_using_speedangle', 'pitch_name')
              
              
            ),
            
            public = list(
              player_type = NA,
              team = NA,
              hfSea = NA,
              params = list(),
              url = NA,
              data = NA,
            
            
              initialize = function(player_type = NULL, team = NULL,
                                  season = NULL, params = list()){
              
                message('Setting parameters for request')
                self$player_type <<- player_type
                self$team <<- team
                self$hfSea <<- season
                self$params <<- params
                self$data <<- NA
                self$url <<- private$base_url
                self$points <<- list(top = 3.5,
                                  bot = 1.6,
                                  in_ = -0.85,
                                  out = 0.85)
                self$zone <<-  data.frame(
                                    x=c(self$in_, self$in_, self$out, self$out, self$in_),
                                    y=c(self$bot, self$top, self$top, self$bot, self$bot)
                                  )
                
                
                message('Combining all params')
                
                all_params <- c(list(player_type = player_type,
                                  team = self$team,
                                  hfSea = self$hfSea),
                               params)
                print(self$url)
              
                
                for(a in names(all_params)){
                  message('Adding ', a, ' to url as parameter' )  
                  self$url <<- param_set(self$url, a, 
                                       all_params[[a]])
                  message('Done with ', a)
                  
                }
                
                self$data <<- self$get_data(self$url)
                self$data <<- self$clean_data()
                
              message('Stat Cast instance initiated for ', all_params)  
                
        },
      
      
      get_data = function(url = self$url){
        
        file_name <- tempfile()
        
        data <- download.file(url,  file_name)
        
        df <- read.csv(file_name)
        
        unlink(file_name)
        
        self$data <<- df
      
        
        
      },
      
      
      clean_data = function(){
        
        data <- self$data
        
        print(NCOL(data))
        
        for(a in names(data)){
          data[,a][data[,a] == 'null' | data[,a] == ''] <- NA
        }
        
        print(NCOL(data))
        
        dep_index <- which(!str_detect(names(data), '_deprecated'))
        
        data <- data[,dep_index]
        
        if(param_get(self$url, 'player_type') == 'pitcher'){
          
          data<- data[,names(data) %in% private$pitch_fields]
        }
        
        self$data <<- data
        
      },
      
      create_avg_pitch_chart = function(player, values, filter_pitch){
        
        if(!values %in% c('release_speed', 'release_spin_rate')){
          
          stop('values much be either release_speed or release spin rate')
          
        }
        
        title <- paste(player,' ', filter_pitch)
        
        message('Creating plot for ', player, "'s", " ", filter_pitch, " ", values)
        
        if(values == 'release_speed'){
          
          y_lab <- 'Velocity (MPH)'
          x_lab <- 'Game Date'
          
          data <- self$data %>% filter(pitch_name == filter_pitch &
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
        
        if(values == 'release_spin_rate'){
          
          y_lab <- 'Spin Rate'
          x_lab <- 'Game Date'
          
          data <- self$data %>% filter(pitch_name == filter_pitch &
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
        
        
      },
      
      create_pitch_scatter = function(player, filter_pitch = NULL){
        
        if(!is.null(filter_pitch)){
          
          data <- self$data %>% filter(pitch_name == filter_pitch &
                                    player_name == player)
        }else{
          
          data <- self$data %>% filter(player_name == player)
          
        }
        
        
        plot(x = data$plate_x, y = data$plate_z, type = 'p')
        
      }
      
      
      
      
      
      
      
      ))
      
      
                          
                          
                          
                          
                          