data_fetcher <- R6::R6Class(
  'data_fetcher',
  private = list(
    base_url = readLines('config/base-url.txt'),
    
    set_field_attr = function(){
      
      self$x_limit <<- xlim(0,250)
      self$y_limit <<- ylim(-250, 0)
      self$right_line <<- geom_segment(x=128, xend = 33, y=-208, yend = -100)
      self$left_line <<- geom_segment(x=128, xend = 223, y=-208, yend = -100)
      self$fence <<- geom_curve(x = 83, xend = 173, y = -155, yend = -156,
                                curvature = -.65, linetype = "dotted")
      self$infield <<- geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                                  curvature = -.65)
      
      self$spray_chart_base <<-
        ggplot(self$data, aes(x = as.numeric(hc_x), y = -as.numeric(hc_y))) +
        geom_point(fill = "blue",
                   color = "grey20", alpha = .75,
                   shape = 21, size = 1, stroke = 1)+
        self$x_limit+
        self$y_limit+
        self$infield+
        self$fence+
        self$right_line +
        self$left_line
      
    },
    
    set_strike_zone = function(){
      
      self$strike_zone <<-
        geom_rect(
          xmin = -.85,
          xmax = .85,
          ymin = 1.6,
          ymax = 3.4,
          color = 'Black',
          fill = NA
        )
      
      self$zone_axes <<-
        list(x = scale_x_continuous(
          limits = c(-1.5, 1.5),
          breaks = seq(-1.5, 1.5, .5)
        ),
        y = scale_y_continuous(limits = c(1, 4),
                               breaks = seq(1, 4, .5)))
      
      
    }
  ),
  public = list(
    player_type = NA,
    team = NA,
    hfSea = NA,
    params = list(),
    player_name=NA,
    batters_lookup=NA,
    pitchers_lookup=NA,
    url = NA,
    data = NA,
    strike_zone = NA,
    zone_axes = NA,
    zone = NA,
    x_limit = NA,
    y_limit = NA,
    right_line= NA,
    left_line = NA,
    fence = NA,
    infield = NA,
    spray_chart_base=NA,
    
    initialize = function(download = TRUE,
                          player_name = NULL,
                          player_type = NULL,
                          batters_lookup = NULL,
                          pitchers_lookupp = NULL,
                          team = NULL,
                          season = NULL,
                          params = list()) {
      message('Setting parameters for request')
      self$player_type <<- player_type
      self$team <<- team
      self$player_name <<- player_name
      self$batters_lookup <<- batters_lookup
      self$pitchers_lookup <<- pitchers_lookupp
      self$hfSea <<- paste0(season, '%7C')
      self$params <<- params
      self$data <<- NA
      self$url <<- private$base_url
      
      message('Combining all params')
      
      all_params <- c(list(
        player_type = self$player_type,
        team = self$team,
        'batters_lookup%5b%5d' = self$batters_lookup,
        'pitchers_lookup%5b%5d' = self$pitchers_lookup,
        hfSea = self$hfSea
      ),
      self$params)
      
      all_params <- all_params[!unlist(map(all_params, is.null))]
      
      
      for (a in names(all_params)) {
        message('Adding ', a, ' to url as parameter')
        print(a)
        self$url <- param_set(self$url, a,
                               all_params[[a]])
        message('Done with ', a)
        
      }
      
      if (download) {
        self$data <<- self$get_data(self$url)
        self$data <<- self$clean_data()
        
      }
      private$set_field_attr()
      private$set_strike_zone()
      
      message('Stat Cast instance initiated for ', all_params)
      
    },
    
    
    get_data = function(url = self$url) {
      file_name <- tempfile()
      
      data <- download.file(url,  file_name)
      
      df <- read.csv(file_name)
      
      unlink(file_name)
      
      self$data <<- df
      
      
    },
    
    
    clean_data = function() {
      data <- self$data
      
      for (a in names(data)) {
        data[, a][data[, a] == 'null' | data[, a] == ''] <- NA
      }
      
      dep_index <- which(!str_detect(names(data), '_deprecated'))
      
      data <- data[, dep_index]
      
      data <- data %>%
        mutate(
          plate_x = as.double(plate_x),
          plate_z = as.double(plate_z),
          in_zone = plate_x >= -.85 & plate_x <= .85 &
            plate_z >= 1.6 & plate_z <= 3.4,
          outs_made = ifelse(str_detect(data$events, 'double_play'), 2,
                             ifelse(str_detect(data$events, 'tripple_play'), 3,
                              ifelse(data$events %in% c('field_out', 'force_out',
                                                "fielders_choice","strikeout",
                                                "sac_fly"), 1, 0)))
        )
      
      self$data <<- data
      
      if(NROW(data) == 0){
        warning("No rows of data returned, make sure you are requesting data for a season that has started")
      }
      
      return(data)
      
    },
    
    set_params = function(params = list()) {
      if (class(params) != 'list') {
        stop(
          'Url parameters must be passed as a named list \n
          See https://www.fangraphs.com/tht/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/'
        )
        
      }
      
      for (a in names(params)) {
        message('Adding ', a, ' to url as parameter')
        self$url <<- param_set(self$url, a,
                               all_params[[a]])
        message('Done with ', a)
        
      }
      
    },
    
    
    save_data = function(path) {
      if (is.na(self$data)) {
        download.file(url = self$url,
                      destfile = path)
      } else{
        write.csv(self$data, file = path)
      }
      
      
      message("Data saved to", path)
      
    }
  )
)