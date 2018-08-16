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
              strike_zone = NA,
              zone_axes = NA,
              zone = NA,
              plots = list(),
              mix = list(),
            
            
              initialize = function(download = TRUE, player_type = NULL, team = NULL,
                                  season = NULL, params = list()){
              
                message('Setting parameters for request')
                self$player_type <<- player_type
                self$team <<- team
                self$hfSea <<- paste0(season, '%7C')
                self$params <<- params
                self$data <<- NA
                self$url <<- private$base_url
                self$strike_zone <<- geom_rect(xmin = -.85, xmax = .85, 
                                               ymin = 1.6, ymax = 3.4,
                                               color = 'Black', fill = NA)
                
                self$zone_axes <<- list(x = scale_x_continuous(limits = c(-1.5,1.5), 
                                                          breaks = seq(-1.5,1.5,.5)),
                                   y = scale_y_continuous(limits = c(1,4), 
                                                          breaks = seq(1,4,.5)))
                
                
                message('Combining all params')
                
                all_params <- c(list(player_type = player_type,
                                  team = self$team,
                                  hfSea = self$hfSea),
                               params)
                
                
                for(a in names(all_params)){
                  message('Adding ', a, ' to url as parameter' )  
                  self$url <<- param_set(self$url, a, 
                                       all_params[[a]])
                  message('Done with ', a)
                  
                }
                
                if(download){
                
                  self$data <<- self$get_data(self$url)
                  self$data <<- self$clean_data()
                  
                }

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
        
        data <- data %>% 
          mutate(plate_x = as.double(plate_x),
                 plate_z = as.double(plate_z),
                 in_zone = plate_x >= -.85 & plate_x <= .85 &
                   plate_z >= 1.6 & plate_z <= 3.4)
        
       
        
        self$data <<- data
        
        return(data)
        
      },
      
      set_params = function(params = list()){
        
        if(class(params) != 'list'){
          
          stop('Url parameters must be passed as a named list \n
               See https://www.fangraphs.com/tht/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/')
          
        }
        
        for(a in names(params)){
          message('Adding ', a, ' to url as parameter' )  
          self$url <<- param_set(self$url, a, 
                                 all_params[[a]])
          message('Done with ', a)
          
        }
        
      },
        
      
      save_data = function(path){
        
        if(is.na(self$data)){
        
          download.file(url = self$url, 
                      destfile = path)
        }else{
          
          write.csv(self$data, file = path)
        }
        
        
        message("Data saved to", path)
        
      },
      
      pitch_timeseries_group = function(player, values, filter_pitch){
        
        
        data <- self$data %>% filter(pitch_name == filter_pitch &
                                       player_name == player) %>%
          mutate(game_date = as.Date(game_date),
                 release_speed = as.numeric(release_speed),
                 release_spin_rate = as.numeric(release_spin_rate))
        data$vals <- data[,values]
        
        data <- data %>%
          group_by(game_date) %>%
          mutate(min_val = min(as.numeric(vals)),
                 max_val = max(as.numeric(vals)),
                 avg_val = mean(as.numeric(vals)))
        
        
        data <- data %>% 
          select(game_date, min_val, max_val, avg_val) %>% 
          distinct(.keep_all = TRUE) %>%
          gather(min_val, max_val, avg_val, 
                 key = 'def', value = 'vals')
        
        return(data)
        
        
      },
      
      compile_pitch_mix = function(player){
        
        if(self$player_tye != 'pitcher'){
          
          stop("Cant get pitch mix, data is for hitters")
          
          
        }
        
        data <- self$data %>% filter(player_name == player) %>%
          filter(pitch_type != 'IN')
        
        data$pitch_type <- ifelse(data$pitch_type == 'FT', 'FF', data$pitch_type)
        
        mix <- data %>% group_by(game_date) %>% 
          select(game_date, pitch_type) %>%
          mutate(total_pitches = n())
        
        mix <- mix %>%
          group_by(game_date, pitch_type) %>%
          mutate(of_pitch = n()) %>%
          ungroup() %>%
          distinct(.keep_all = TRUE)
        
        mix$pitch_percentage <- mix$of_pitch / mix$total_pitches
        
        self$mix[[player]] <<- mix
        
        return(mix)
        
      },
      
      create_avg_pitch_chart = function(player, values, filter_pitch, save_plot = FALSE){
        
        if(!values %in% c('release_speed', 'release_spin_rate')){
          
          stop('values much be either release_speed or release spin rate')
          
        }
        
        data <- self$pitch_timeseries_group(player, values, filter_pitch)
        
        title <- ggtitle(paste(player,' ', filter_pitch))
        
        months <-  unique(sort(floor_date(as.Date(data$game_date), 'month')))
        
        message('Creating plot for ', player, "'s", " ", filter_pitch, " ", values)
        
        if(values == 'release_speed'){
          
          message('Setting Title')
          
          x_lab <- xlab('Velocity (MPH)')
          y_lab <- ylab('Game Date')
        }
        
        if(values == 'release_spin_rate'){
          
          y_lab <- ylab('Spin Rate')
          x_lab <- xlab('Game Date')
          
          
        }
        
        
        plot <- ggplot(data, aes(x = game_date, y = vals, group = game_date)) + 
          geom_boxplot(width = .0005) +
          geom_point(aes(x = game_date, y = vals)) +
          x_lab + y_lab +
          ggtitle(title) +
          scale_x_date(date_breaks = "1 month")
        
        if(save_plot){
          
          self$plots[[title$title]] <- plot
          
        }
        
        return(plot)
        
        
      },
      
      create_pitch_scatter = function(player, filter_pitch = NULL, save_plot = FALSE){
        
        if(!is.null(filter_pitch)){
          
          title <- ggtitle(paste(player,' ', filter_pitch,':', 'Pitch scatter'))
          
          data <- self$data %>% filter(pitch_name == filter_pitch &
                                    player_name == player)
        }else{
          title <- ggtitle(paste(player,' ', 'Pitch scatter'))
          
          data <- self$data %>% filter(player_name == player)
          
        }
        
        data <- data %>% select(pitch_type, plate_x, plate_z, in_zone)
        
        plot <- ggplot(data, aes(x = plate_x, y = plate_z)) +
          geom_jitter(aes(col = in_zone))+
          self$strike_zone+
          self$zone_axes$x+
          self$zone_axes$y 
        
        if(save_plot){
          
          
          self$plots[[title$title]] <- plot
          
        }
        
        return(plot)
        
      },
      
     
      
      
      show_pitch_mix = function(player, save_plot = TRUE, min = NULL, max = NULL){
        
        mix <- compile_pitch_mix(player)
        
        title <- ggtitle(paste(player, 'Pitch Mix', setp = ' '))
      
        
        plot <- ggplot(mix, aes(x = game_date, y = pitch_percentage, col = pitch_type)) + 
          geom_point() +
          geom_line() +
          title
        
        
        if(save_plot){
          
          self$plots[[title$title]] <<- plot
          
        }
        
        return(plot)
        
        
      }

      
      
      ))
      
      
                          
                          
                          
                          
                          
