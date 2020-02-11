get_roster <- function(team,teams, use_year){
  message('Getting teamid')
  
  team_id=teams %>% filter(team_name == team) %>%
    select(team_id) %>% unlist()
  
  message('Checking for time of year adjustment')
  if(month(today()) < 4 & year(today()) == use_year){
    message('Its the preseason, using previous year')
    use_year = as.numeric(use_year) -1
    message(use_year)
  }
  
  message("Preparing data request")
  
  base_url="http://lookup-service-prod.mlb.com/json/named.roster_team_alltime.bam?start_season=1&end_season=&team_id="
  
  url <- param_set(base_url, 'start_season', paste0("'",use_year,"'"))
  url <- param_set(url, 'end_season', paste0("'",use_year,"'"))
  url <- param_set(url, 'team_id', paste0("'",team_id,"'"))
  
  message('Sending request to ', url)
  data <- jsonlite::fromJSON(RCurl::getURL(url, encoding='latin1'))
  
  data <- data$roster_team_alltime$queryResults$row %>%
    mutate(
      position_txt=primary_position,
      name_display_first_last=name_first_last,
      position=if_else(position_txt=='P','Pitchers','Hitters')
    ) %>%
    select(name_display_first_last, player_id, position)
  
  
  return(data)
  
}