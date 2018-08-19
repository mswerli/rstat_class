source('src/init.R')
source("src/stat_class.R")


pitchers <- stat_cast$new(player_type = 'pitcher', 
                          team = 'NYM',
                          season = 2018)

pitchers$create_avg_pitch_chart(player = 'Jacob deGrom', 
                                values = 'release_speed',
                                filter_pitch = '4-Seam Fastball')

pitchers$create_pitch_scatter(player = 'Jacob deGrom',
                              filter_pitch = '4-Seam Fastball')

