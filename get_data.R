library(urltools)
library(stringr)
library(stringi)
library(purrr)
library(DBI)
library(lubridate)
library(RPostgreSQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(R6)
source('src/utils.R')
options(stringsAsFactors = FALSE)


pitch_fields <- c('pitch_type', 'game_date', 'release_speed', 'release_pos_x', 
            'release_pos_z', 'player_name', 'batter', 'pitcher', 'description', 
            'zone', 'p_throws', 'home_team', 'away_team','type', 'hit_location', 
            'bb_type', 'balls', 'strikes', 'pfx_x', 'pfx_z', 'plate_x', 'plate_z', 
            'on_3b', 'on_2b', 'on_1b', 'outs_when_up', 'hc_x', 'hc_y', 'vx0', 'vy0', 
            'vz0', 'ax', 'ay', 'az', 'sz_top', 'sz_bot', 'hit_distance_sc', 'launch_speed', 
            'launch_angle', 'release_spin_rate', 'release_extension', 'release_pos_y', 
            'estimated_ba_using_speedangle', 'estimated_woba_using_speedangle', 'pitch_name')


parms = list(team = 'ATL',
             hfSea = '2018%7C',
             player_type = 'pitcher')
             

pitch_data2 <- get_data(parms)

pitch_data2 <- clean_data(pitch_data2)

pitch_data2 <- pitch_data2[,names(pitch_data) %in% pitch_fields]

parms$player_type<- 'hitter'

hit_data <- get_data(parms)

hit_data <- clean_data(data)


