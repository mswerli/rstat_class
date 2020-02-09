data_fetcher <- R6::R6Class(
  'data_fetcher',
  private = list(
    base_url = readLines('config/base-url.txt')
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
      
      message('Combining all params')
      
      all_params <- c(list(
        player_type = self$player_type,
        team = self$team,
        batters_lookup = self$batters_lookup,
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
      
      print(NCOL(data))
      
      for (a in names(data)) {
        data[, a][data[, a] == 'null' | data[, a] == ''] <- NA
      }
      
      print(NCOL(data))
      
      dep_index <- which(!str_detect(names(data), '_deprecated'))
      
      data <- data[, dep_index]
      
      data <- data %>%
        mutate(
          plate_x = as.double(plate_x),
          plate_z = as.double(plate_z),
          in_zone = plate_x >= -.85 & plate_x <= .85 &
            plate_z >= 1.6 & plate_z <= 3.4
        )
      
      self$data <<- data
      
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