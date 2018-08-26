source('src/init.R')
source("src/stat_class.R")


hitters <- stat_cast$new(player_type = 'hitter', 
                          team = 'ATL',
                          season = 2018)
data <- hitters$data %>% filter(player_name == 'Freddie Freeman')
