source('data_fetcher.R')
source('hitter_class.R')
source('pitcher.R')
source('src/init.R')
options(stringsAsFactors = FALSE)

player_hitter <- batter_instance$new(
  player_type = 'batter',
  player_name="Dansby Swanson",
  batters_lookup = get_roster("Atlanta Braves", teams, 2019) %>%
    filter(name_display_first_last ==  "Dansby Swanson") %>%
    select(player_id) %>% unlist() %>%
    as.character(),
  team=NULL,
  season=2019)

player_hitter$generate_outcome_timeseries(stat='woba_value')

########
# Stat #
########
##woba_value --> WOBA
##iso_value --> ISO
##launch_speed --> Exit Velocity
##launch_angle --> Launch Angle
##estimated_woba_using_speedangle --> Estimated WOBA

##############
# Pitch Type #
##############
## Unique values of pitch name

###################
## Pitcher Throws##
###################
player_hitter$generate_spary_chart(bucket = 'p_throws', 
                                   filters = list(in_play = TRUE,
                                                  events = c('single','double')))

## Inputs -- Filters
  ## Pithcer Handedness 
  ## Pitch Type
  ## Events
## Group 1 -- Attributes (color)
## Group 2 --- Attributes (shape)



## Button
## When button pressed
  ### Check all inputs
  ### For each input add component to base chart
    ### Means there needs to be a private method for each input.Run for each item in input
  ### For each filter
 
  
  
