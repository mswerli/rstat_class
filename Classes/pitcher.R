pitcher_instance <- R6::R6Class(
  "Pitcher",
  inherit = data_fetcher,
  public = list(
    
    columns = unlist(jsonlite::read_json("config/columns.json")[['pitch_fields']]),
    data=NA,
    pitch_mix_game=NA,
    pitch_mix_agg=NA,
    pitch_names=NA,
    visualizations=c('Pitch Mix', 'Pitch Heat Map', "Pitch Time Series"),
    
    initialize = function(download = TRUE,
                           player_type = 'pitcher',
                           player_name= NULL,
                           pitchers_lookup = NULL,
                           team = NULL,
                           season = NULL,
                           params = list()){
      
      message("Getting data for ", season, "for ", player_name)
      
      super$initialize(download =download,
                       player_type=player_type,
                       player_name=player_name,
                       batters_lookup=NULL,
                       pitchers_lookup=pitchers_lookup,
                       team=team,
                       season=season,
                       params=params)
      
      self$data <<- self$data[, names(self$data) %in% self$columns]
      self$data <<- self$clean_data()
      self$pitch_names <<- self$get_pitch_types()
      
      
    },
    
    get_pitch_types = function(){
      
      pitches <- self$data[which(!is.na(self$data$pitch_name)),]
      pitches <- unique(pitches$pitch_name)
      
      return(pitches)
      
      
    },
    pitch_timeseries_group = function(values, filter_pitch) {
      data <- self$data %>% filter(pitch_name == filter_pitch) %>%
        mutate(
          game_date = as.Date(game_date),
          release_speed = as.numeric(release_speed),
          release_spin_rate = as.numeric(release_spin_rate)
        )
      data$vals <- data[, values]
      
      data <- data %>%
        group_by(game_date) %>%
        mutate(
          min_val = min(as.numeric(vals)),
          max_val = max(as.numeric(vals)),
          avg_val = mean(as.numeric(vals))
        )
      
      
      data <- data %>%
        select(game_date, min_val, max_val, avg_val) %>%
        distinct(.keep_all = TRUE) %>%
        gather(min_val, max_val, avg_val,
               key = 'def', value = 'vals')
      
      return(data)
      
      
    },
    
    compile_pitch_mix = function() {

      data <- self$data %>%
        filter(pitch_type != 'IN')
      
      data$pitch_type <-
        ifelse(data$pitch_type == 'FT', 'FF', data$pitch_type)
      
      mix <- data %>% group_by(game_date) %>%
        select(game_date, pitch_type) %>%
        mutate(total_pitches = n())
      
      mix <- mix %>%
        group_by(game_date, pitch_type) %>%
        mutate(of_pitch = n()) %>%
        ungroup() %>%
        distinct(.keep_all = TRUE)
      
      mix$pitch_percentage <- mix$of_pitch / mix$total_pitches
      
      self$pitch_mix_game <<- mix
      
    },
    
    compile_mix_summary = function(){
      
      if(is.na(self$pitch_mix_game)){
        self$compile_pitch_mix()
      }
      
      data <- self$pitch_mix_game %>% mutate(month = floor_date(unit = 'month',x= as.Date(game_date))) %>%
        select(-game_date, -pitch_percentage, -total_pitches) %>%
        group_by(month, pitch_type) %>%
        mutate(of_pitch = sum(of_pitch, na.rm = TRUE)) %>%
        distinct(.keep_all = TRUE) %>%
        ungroup() %>%
        group_by(month) %>%
        mutate(total_pitches = sum(of_pitch)) %>%
        ungroup() %>%
        distinct(.keep_all = TRUE) %>%
        mutate(pitch_percentage = of_pitch/total_pitches) %>%
        select(pitch_type, month, pitch_percentage)
      
      self$pitch_mix_agg <<- data
    },
    
    create_avg_pitch_chart = function(values, filter_pitch, save_plot = FALSE) {
      
      if (!values %in% c('release_speed', 'release_spin_rate')) {
        stop('values much be either release_speed or release spin rate')
      }
      
      data <-self$pitch_timeseries_group(values, filter_pitch)
      
      title <- ggtitle(paste(self$player_name, ' ', filter_pitch))
      
      months <-
        unique(sort(floor_date(as.Date(data$game_date), 'month')))
      
      message('Creating plot for ', self$player_name, "'s", " ", filter_pitch, " ", values)
      
      if (values == 'release_speed') {
        message('Setting Title')
        
        x_lab <- xlab('Velocity (MPH)')
        y_lab <- ylab('Game Date')
      }
      
      if (values == 'release_spin_rate') {
        y_lab <- ylab('Spin Rate')
        x_lab <- xlab('Game Date')
        
        
      }
      
      plot <-
        ggplot(data, aes(x = game_date, y = vals, group = game_date)) +
        geom_boxplot(width = .0005) +
        geom_point(aes(x = game_date, y = vals)) +
        x_lab + y_lab +
        ggtitle(title) +
        scale_x_date(date_breaks = "1 month")
      
      # if (save_plot) {
      #   self$plots[[title$title]] <- plot
      #   
      # }
      
      return(plot)
      
      
    },
    
    create_pitch_scatter = function(filter_pitch = NULL,
                                    save_plot = FALSE) {
      if (!is.null(filter_pitch)) {
        title <-
          ggtitle(paste(self$player_name, ' ', filter_pitch, ':', 'Pitch scatter'))
        
        data <- self$data %>% filter(pitch_name == filter_pitch)
      } else{
        title <- ggtitle(paste(self$player_name, ' ', 'Pitch scatter'))
        
        data <- self$data
        
      }
      
      data <-
        data %>% select(pitch_type, plate_x, plate_z, in_zone)
      
      plot <- ggplot(data, aes(x = plate_x, y = plate_z)) +
        geom_bin2d(bins = 12) +
        scale_fill_continuous(low = "blue", high = "red") +
        self$strike_zone +
        self$zone_axes$x +
        self$zone_axes$y
      
      # if (save_plot) {
      #   self$plots[[title$title]] <- plot
      #   
      # }
      
      return(plot)
      
    },
    
    show_pitch_mix = function(save_plot = TRUE,
                              min = NULL,
                              max = NULL) {
  
      if(is.na(self$pitch_mix_agg)){
        self$compile_mix_summary()
      }
      
      title <- ggtitle(paste(self$player_name, 'Pitch Mix', setp = ' '))
      
      
      plot <-
        ggplot(data=self$pitch_mix_agg,
               aes(x=month, y=pitch_percentage, colour=pitch_type)) +
        geom_line()
      
      
      # if (save_plot) {
      #   self$plots[[title$title]] <<- plot
      #   
      # }
      
      return(plot)
      
    }
    

  )
)
