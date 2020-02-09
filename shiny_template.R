library(shiny)
source('src/init.R')
source('data_fetcher.R')
source('pitcher.R')
get_roster <- function(team,teams){
  
  base_url='http://lookup-service-prod.mlb.com/json/named.roster_40.bam?'
  
  team_id=teams %>% filter(team_name == team) %>%
    select(team_id) %>% unlist()
  
  url <- paste0(base_url,"team_id='",team_id[[1]],"'")
  
  file_name <- tempfile()
  
  download.file(url,  file_name)
  
  data <- jsonlite::fromJSON(file_name)
  
  data <- data$roster_40$queryResults$row %>%
    mutate(position=if_else(position_txt=='P','Pitchers','Hitters')) %>%
    select(name_display_first_last, player_id, position)
  
  return(data)
  
}

teams <- jsonlite::read_json('config/team_ref.json', simplifyVector = TRUE)
ui <- fluidPage(
  
  selectInput('League', 
              "Pick a league", 
              c('AL','NL'),
              selected = 'AL'),
  
  selectInput("Team", 
              "Which Team", 
             teams$team_name),
  
  selectInput("Position", 
              "Position", 
              c('Hitters','Pitchers'),
              "Hitters"),
  
  selectInput("Player",
              "Player",
              c("")),
  
  selectInput("Season",
              "Season",
              c("2017","2018","2019","2020"),
              "2019"),
  
   actionButton("get_data",
                "get_data")
  
  
  
  
)

server <- function(input, output,session) {
  
  observe({
    updateSelectInput(
      session,
      "League",
      choices =  c('AL','NL'))
  })
  
  observe({
    updateSelectInput(
      session,
      "Team",
      choices = teams %>% filter(league==input$League) %>% 
        select(team_name) %>% unlist() %>% as.character())
  })
  
  observe({
    updateSelectInput(
      session,
      "Position",
      choices = c("Hitters","Pitchers"))
  })
  
  observe({
    updateSelectInput(
      session,
      "Player",
      choices = get_roster(input$Team, teams) %>%
        filter(position == input$Position) %>%
        select(name_display_first_last) %>% unlist() %>% 
        as.character())
  })
  
  observeEvent(input$get_data, {

    if(input$Position == 'Pitchers'){
      
      message('Getting pitching data')

      pitcher <- pitcher$new(
        player_type = 'pitcher',
        player_name=input$Player,
        batters_lookup = NULL,
        pitchers_lookup=get_roster(input$Team, teams) %>%
          filter(name_display_first_last == input$Player) %>%
          select(player_id) %>% unlist() %>%
          as.character(),
        team=input$Tean,
        season=input$Season)
      
      print(NROW(pitcher$data))
    }
  })
  
}

shinyApp(ui = ui, server = server)