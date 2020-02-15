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
                          pitchers_lookup = NULL,
                          team = NULL,
                          season = NULL,
                          params = list()){
      
      message("Getting data for ", season, "for ", player_name)
      
      super$initialize(download =download,
                       player_type=player_type,
                       player_name=player_name,
                       batters_lookup=batters_lookup,
                       pitchers_lookup=pitchers_lookup,
                       team=team,
                       season=season,
                       params=params)
    },
    
    compile_batted_timeseries = function(measure){
      
      message("Calculating 30 day running average of ", measure)
      
      data <- player_hitter$data %>% 
        select(game_date, !!sym(measure)) %>%
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
    
    generate_outcome_timeseries = function(measure){
      
      data <- compile_batted_timeseries(mesaure )
      
      plot <- ggplot() +
        geom_line(data = data, aes(x = as.Date(game_date), y = value))  +
        scale_x_date(date_breaks = "1 month")
      
      return(plot)
    }
    
    generate_spary_chart = function(filters=list(), bucket=NA, shape=NA){
      
      data <- self$data
      names(data)
      
      for(f in names(filters)){
        message('Filtering for ',f)
        data <- private$spary_filter(data, f, filters[[f]] )
      }
      
      
      plot <- self$spray_chart_base
      
      if(!is.na(bucket) & !is.na(shape)){
        
        row_check <- NROW(data)
        data <- data[which(!is.na(data[bucket])),]
        data <- data[which(!is.na(data[shape])),]
        
        if(NROW(data) < row_check){
          
          message("Columns ", 
                  bucket," ", shape, 
                  " used for colors and shapes had NAs. ", 
                  row_check-NROW(data), " rows removed")
          
        }
        data <- data[c('hc_x','hc_y', shape, bucket)]
        plot$data <- data
        plot$layers[[1]] <- geom_point(alpha = .75, 
                                       size = 1, stroke = 1,
                                       aes_string(color = bucket, shape = shape))
      }
      
      
      
      if(!is.na(bucket) & is.na(shape)){
        
        row_check <- NROW(data)
        data <- data %>% filter(!is.na(sym(bucket)))
        
        if(NROW(data) < row_check){
          
          message("Column ", 
                  bucket, 
                  " used for colors had NAs. ", 
                  row_check-NROW(data), " rows removed")
          
        }
        
        plot$layers[[1]] <- geom_point(alpha = .75,
                                       shape = 21, size = 1, stroke = 1,
                                       aes(color = bucket))
        
      }
      
      if(!is.na(shape) & is.na(bucket)){
        
        row_check <- NROW(data)
        data <- data %>% filter(!is.na(!!sym(shape)))
        
        if(NROW(data) < row_check){
          
          message("Column ", 
                  shape, 
                  " used for shape had NAs. ", 
                  row_check-NROW(data), " rows removed")
          
        }
        
        plot$layers[[1]] <- geom_point(fill = "blue",
                                       color = "grey20", alpha = .75,
                                       size = 1, stroke = 1, aes(shape = bucket))
        
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

