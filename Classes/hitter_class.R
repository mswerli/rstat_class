batter_instance <- R6::R6Class(
  "Hitter",
  inherit = data_fetcher,
  public = list(
    
    columns = unlist(jsonlite::read_json("config/columns.json")[['hit_fields']]),
    data = NA,
    spray_base=NA,
    visualizations = NA,
    
    initialize = function(download = TRUE,
                          player_type = 'batter',
                          player_name= NULL,
                          batters_lookup = NULL,
                          team = NULL,
                          season = NULL,
                          params = list()){
      
      message("Getting data for ", season, "for ", player_name)
      
      super$initialize(download =download,
                       player_type=player_type,
                       player_name=player_name,
                       batters_lookup=batters_lookup,
                       team=team,
                       season=season,
                       params=params)
    },
    
    compile_batted_timeseries = function(measure, p_hand=c('L',"R"), pitch){
      
      message("Calculating 30 day running average of ", measure)
      
      data <- player_hitter$data %>% 
        filter(p_throws %in% p_hand)
      
      if(pitch != 'all'){
        
        data <- data %>%
          filter(pitch_name %in% pitch)
        
      }
      
      data <- data %>%select(game_date, !!sym(measure), p_throws, pitch_type) %>%
        mutate(game_date = as.Date(game_date),
               value = as.numeric(!!sym(measure))) %>%
        select(-!!sym(measure)) %>%
        filter(!is.na(value))
      
      data <-data[order(data$game_date),]
      
      data <- data.frame(game_date = data$game_date,
                         value = rollmean(x = data$value, k=30, fill = FALSE, align = 'right')) %>%
        mutate(ord=seq(n())) %>%
        filter(ord > 29)
      
    },
    
    generate_outcome_timeseries = function(stat, p_hand = c('L','R'), pitch = 'all'){
      
      message('Getting timeseries data')
      data <- self$compile_batted_timeseries(measure = stat, p_hand, pitch)
      
      message('Building Plot')
      plot <- ggplot() +
        geom_line(data = data, aes(x = as.Date(game_date), y = value))  +
        scale_x_date(date_breaks = "1 month")
      
      return(plot)
    },
    
    generate_spary_chart = function(filters=list(), bucket='None'){
      
      message("Building spray chart")
      data <- self$data %>% filter(in_play)

      for(f in names(filters)){
        message('Filtering for ',f)
        data <- private$spary_filter(data, f, filters[[f]] )
      }
      
      message("Data has been filtered")
      
      plot <- self$spray_chart_base
      cols <- c('hc_x','hc_y')
      
      if(bucket != 'None'){
        
        message('Grouping data by ', bucket)
        
        cols <- c(cols, bucket)
        row_check <- NROW(data)
        data <- data[which(!is.na(data[bucket])),]
        
        message('Removed NAs from ', bucket)
        
        if(NROW(data) < row_check){
          
          message("Columns ", 
                  bucket, 
                  " used for colors had NAs. ", 
                  row_check-NROW(data), " rows removed")
          
          
          print('Columns are')
          
        }
        
        message('Limiting columns to ', cols)
        data <- data[cols]
        plot$data <- data
        plot$layers[[1]] <- geom_point(alpha = .75, 
                                       size = 1, stroke = 1,
                                       aes_string(color = bucket))
      }
      
      message('Returning plot')
      return(plot)
      
    }
  ),
  private = list(
    
    complie_inputs = function(){
      self$pitch_names <<- unique(self$data$pitch_name)
      self$outcomes <<- unique(self$data[which(!is.na(self$data$hc_x)),]$events)
      self$outs_made <<- unque(self$data$outs_made)
      
    },
    
    spary_filter = function(data, col, vals){
      message(names(vals))
      
      data = data %>% filter(!!sym(col) %in% vals)
      return(data)
    }
    
  )
)

