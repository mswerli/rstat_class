player <- R6::R6Class(
  "Player",
  public = list(
    
    conn=NA,
    name=NA,
    player_id=NA,
    years_played=NA,
    teams_played_for=NA,
    height=NA,
    weight=NA,
    age=NA,
    position=NA,
    jersey_number=NA,
    bats=NA,
    throws=NA,
    baseball_card=NA,
    
    initialize = function(player_name=NULL){
      
      self$conn <<- db_interface$initialize()
      self$player_name = player_name
      self$get_attributes()
      self$build_baseball_card()
      
    },
    
    get_attributes = function(){
      
      player_details <- tbl(con, in_schema('rosters', 'players')) %>%
        filter(name_display_first_last == player_name) %>%
        select(player_id, height_feet, height_inches, weight, 
               throws, bats, primary_position, age, jersey_number) %>% collect()
      
      self$player_id <<- player_details$player_id
      self$height <<- player_details$height_feet * 12 + player_details$height_inches
      self$weight <<- player_details$weight
      self$age <<- player_details$age
      self$position <<- player_details$primary_position 
      self$jersey_number <<- player_details$jersey_number
      self$bats <<- player_details$bats
      self$throws <<- player_details$throws
      
      participation_detials <- tbl(con, in_schema('rosters', 'historical_rosters')) %>%
        left_join(tbl(con, in_schema('league','teams')), by = 'team_id') %>%
        select(player_id,team_id, stat_years, name_display_full) %>%
        filter(player_id == !!player_details$player_id) %>%
        distinct() %>%
        collect()
      
      self$years_played <<- unique(
        unlist(
          strsplit(
            paste(
              str_replace_all(
                paste(participation_detials$stat_years,sep=','),'-',','
                ),collapse=','
              ),
            fixed = TRUE, ',')
          )
      )
      
      self$teams_played_for <<- participation_detials %>% 
        select(stat_years, name_display_full) %>%
        rename("years" = stat_years,
               "team" = name_display_full)

      
    },
    
    build_baseball_card = function(){
      
      if(primary_position > 1){
        
        data <- tbl(con, in_schema('league', 'yearly_batting_stats')) %>%
          filter(batter == !!(player_id)) %>%
          select(p_throws, stand, year, at_home, at_bats, singles, 
                 doubles, triple, home_runs,hits, home_runs, strikeouts, 
                 walks, babip_numerator, babip_denomenator,total_bases, 
                 line_drives, ground_balls, fly_balls, hit_by_pitch, sac_flys)
        
        overall <- data %>%
          select(-p_throws, -stand, -at_home) %>%
          group_by(year) 
        
        overall <- batting_metrics(overall) %>%
            ungroup() %>%
            select(year,AB, BB, K, HITS, HR, 
                   AVG, OBP, SLG, BABIP, ISO) %>%
            distinct() %>%
            collect()
        
        home_away <- data %>%
          select(-p_throws, -stand) %>%
          group_by(year, at_home) 
        
        home_away <- batting_metrics(home_away) %>%
          ungroup() %>%
          select(year, at_home, AB, BB, K, HITS, HR,
                 AVG, OBP, SLG, BABIP, ISO) %>%
          distinct() %>%
          collect()
        
        handedness <- data %>%
          group_by(year, at_home, p_throws, stand) 
        
        handedness <- batting_metrics(handedness) %>%
          ungroup() %>%
          select(year, p_throws, stand,AB, BB, K, HITS, HR, 
                 AVG, OBP, SLG, BABIP, ISO) %>%
          distinct() %>%
          collect()
        
        self$baseball_card <- list(
          "Yearly" = overall,
          "Splits" = list(
            "Home -- Away" = home_away,
            "Platoon" = handedness
          )
        )
      }
      
      if(self$primary_position == 1){
        
        
        data <- tbl(con, in_schema('league', 'yearly_batting_stats')) %>%
          filter(batter == !!(player_id)) %>%
          select(p_throws, stand, year, at_home, at_bats, singles, 
                 doubles, triple, home_runs,hits, home_runs, strikeouts, 
                 walks, babip_numerator, babip_denomenator,total_bases, 
                 line_drives, ground_balls, fly_balls, hit_by_pitch, sac_flys)
        
        
        ##Ks
        ##BB
        ##K%
        ##GB/FB%
        ##BABIP
        ##SLG
        ##OBP
        ##ISO
        ##AVG FB Velo
        ##K/9
        ##BB/9
        ##K/BB
        ##FIP = ((13*HR)+(3*(BB+HBP))-(2*K))/IP + constant
        
        ##Need to calculate constant FIP Constant = lgERA – (((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIP)
        
      }
    
        
      },
      
      batting_metrics = function(data){
        
        data <- data %>%
          mutate(AB = sum(at_bats),
               BB = sum(walks),
               K = sum(strikeouts),
               HITS = sum(hits),
               HR = sum(home_runs),
               AVG = round(sum(hits)/sum(at_bats),3),
               SLG = round(((sum(singles) + 
                        (sum(doubles) * 2) + 
                        (sum(triple) * 3) + 
                        (sum(home_runs) * 4)) / sum(at_bats)),3),
               OBP = round((sum(walks) + sum(hits) + sum(hit_by_pitch))/(sum(at_bats) + sum(hit_by_pitch) + 
                                                                     sum(walks) + sum(sac_flys)),3),
               BABIP = round((sum(HITS) - sum(HR))/(sum(AB) - sum(K) - sum(HR) + sum(sac_flys)),3),
               ISO = round((SLG - AVG),3)) %>%
          filter(AB > 10)
        
        return(data)
        
      }
  )
)