source('src/init.R')
source('data_fetcher.R')
source('pitcher.R')

folty<- pitcher_instance$new(player_type = 'pitcher',
                    player_name='Mike Foltynewicz',
                        pitchers_lookup=592314,
                        team='ATL',
                        season=2019)

folty$show_pitch_mix()

folty$create_pitch_scatter(filter_pitch = 'Slider')

folty$create_avg_pitch_chart(value='release_speed', 
                             filter_pitch = '4-Seam Fastball')
