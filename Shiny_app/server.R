server <- function(input, output,session) {
  
  current_viz <- NA
  player_type <- NA
  
  ##Start with select inputs hiddent
  
  if(is.na(player_type)){
    
    shinyjs::hideElement(id='viz')
    shinyjs::hideElement(id='gen_viz')
    shinyjs::hideElement(id='p_hand')
    shinyjs::hideElement(id='pitch_type')
    shinyjs::hideElement(id='pitch_measure')
    shinyjs::hideElement(id='stat_type')
    shinyjs::hideElement(id='outcomes')
    shinyjs::hideElement(id='multi_pitch_types')
    shinyjs::hideElement(id='p_hand_multi')
    shinyjs::hideElement(id='filters')
    shinyjs::hideElement(id='group')
    
  }
  
  
  
  ##########################
  ### Data Retrieval Tab ###
  ##########################
  
  
  ##Update teams names based on league selected
  observe({
    updateSelectInput(
      session,
      "Team",
      choices = teams %>% filter(league==input$League) %>% 
        select(team_name) %>% unlist() %>% as.character())
  })
  
  ##Filter list of players based on position selected
  observe({
    updateSelectInput(
      session,
      "Player",
      choices = get_roster(input$Team, teams, input$Season) %>%
        filter(position == input$Position) %>%
        select(name_display_first_last) %>% unlist() %>% 
        as.character())
  })
  
  
  ##Actions to perform whe get data button is clicked
  observeEvent(input$get_data, {
    
    player_type <<- input$Position
    
    
    ######################
    ### Pitching Setup ###
    ######################
    
    if(input$Position == 'Pitchers'){
      
      shinyjs::showElement(id='gen_viz')
      shinyjs::showElement(id='viz')
      shinyjs::hideElement(id='filters')
      shinyjs::hideElement(id='stat_type')
      shinyjs::hideElement(id='outcomes')
      shinyjs::hideElement(id='multi_pitch_types')
      shinyjs::hideElement(id='p_hand_multi')
      shinyjs::hideElement(id='filters')
      shinyjs::hideElement(id='group')
      
      updateSelectInput(
        session,
        "viz",
        choices = c('Pitch Mix', 'Pitch Heat Map', "Pitch Time Series"),
        selected='Pitch Mix'
      )
      
      message('Getting pitching data ', input$team)
      
      ## Create instnace of pitcher class based on inputs
      player_pitcher <<- pitcher_instance$new(
        player_name=input$Player,
        pitchers_lookup=get_roster(input$Team, teams, input$Season) %>%
          filter(name_display_first_last == input$Player) %>%
          select(player_id) %>% unlist() %>%
          as.character(),
        team=teams$team_abbr[which(teams$team_name == input$Team)],
        season=input$Season)
      
      ############################
      ##### Start of Viz Tab #####
      ############################
      
      message("Setting current viz")
      
      ##Set current_viz so we can clear plots when viz selection changes
      
      current_viz <- input$viz
      
      ##Handled empty dfs being returned with messages
      
      if(NROW(player_pitcher$data) == 0){
        showNotification("No data found, make sure the season requested has started",
                         type = 'warning',
                         duration = 5)
      }else{
        showNotification("Data retrieved, proceed to pitcher tab for visualizations",
                         type = 'message',
                         duration = 5)
      }
      
      ## Once we have data for a pitcher, update pitch type drop down
      updateSelectInput(
        session,
        "pitch_type",
        choices = c(player_pitcher$pitch_names, 'All'),
        'All'
      )
    }
    
    
    if(input$Position == 'Hitters'){
      
      shinyjs::showElement(id='gen_viz')
      shinyjs::showElement(id='viz')
      
      player_hitter <<- batter_instance$new(
        player_name=input$Player,
        batters_lookup=get_roster(input$Team, teams, input$Season) %>%
          filter(name_display_first_last == input$Player) %>%
          select(player_id) %>% unlist() %>%
          as.character(),
        team=teams$team_abbr[which(teams$team_name == input$Team)],
        season=input$Season)
      
      updateSelectInput(
        session,
        "viz",
        choices = c("Spray Chart", 'Batting Times Series'),
        selected='Spray Chart'
      )
      
      updateSelectInput(
        session,
        "pitch_type",
        choices = c('All',player_hitter$data$pitch_name),
        'All'
      )
      
      if(NROW(player_hitter$data) == 0){
        showNotification("No data found, make sure the season requested has started",
                         type = 'warning',
                         duration = 5)
      }else{
        showNotification("Data retrieved, proceed to pitcher tab for visualizations",
                         type = 'message',
                         duration = 5)
      }
      
    }
  })
  
  ##When switching between viz options we want to
  ##Remove old plots and show/hide neccesary inputs
  
  observeEvent(input$viz,{
    message('Current viz is ', current_viz)
    
    if(!is.na(current_viz)){
      if(current_viz != input$viz){
        message('Removing plot')
        shinyjs::hide("plot")
      }
    }
    
    
    
    #########################
    ###### Viz Control ######
    ########################
    
    
    if(input$viz =='Pitch Mix'){
      
      shinyjs::hideElement(id='pitch_type')
      shinyjs::hideElement(id='pitch_measure')
    }
    
    if(input$viz =='Pitch Heat Map'){
      
      updateSelectInput(
        session,
        "pitch_type",
        choices = c(player_pitcher$pitch_names, 'All'),
        selected='All'
      )
      
      shinyjs::showElement(id='pitch_type')
    }
    
    if(input$viz =='Pitch Time Series'){
      
      updateSelectInput(
        session,
        "pitch_type",
        choices = c(player_pitcher$pitch_names),
        selected=player_pitcher$pitch_names[1]
      )
      
      shinyjs::showElement(id='pitch_type')
      shinyjs::showElement(id='pitch_measure')
    }
    
    if(input$viz =='Batting Times Series'){
      
      shinyjs::hideElement(id='filters')
      shinyjs::hideElement(id='group')
      shinyjs::showElement(id='pitch_type')
      shinyjs::showElement(id='stat_type')
      shinyjs::showElement(id='p_hand')
      
      updateSelectInput(
        session,
        "pitch_type",
        choices = c(unique(player_hitter$pitch_names)),
        selected=player_hitter$pitch_names[1]
      )
    }
    
    if(input$viz =='Spray Chart'){
      
      shinyjs::showElement(id='multi_pitch_types')
      shinyjs::showElement(id='p_hand_multi')
      shinyjs::showElement(id='outcomes')
      shinyjs::showElement(id='group')
      shinyjs::showElement(id='filters')
      
      updateSelectInput(
        session,
        "multi_pitch_types",
        choices = unique(player_hitter$data$pitch_name),
        selected=unique(player_hitter$data$pitch_name)
      )
      
      updateSelectInput(
        session,
        "outcomes",
        choices = unique(player_hitter$data$events[which(player_hitter$data$in_play)]),
        selected=unique(player_hitter$data$events[which(player_hitter$data$in_play)])
      )
    }
  })
  
  
  ########################
  ###### Viz Button ######
  ########################
  
  observeEvent(input$gen_viz, {
    
    current_viz <<- input$viz
    
    if(input$viz == 'Pitch Mix'){
      
      output$plot <- renderPlot({player_pitcher$show_pitch_mix()})
      shinyjs::show("plot")
      
    }
    
    if(input$viz == 'Pitch Heat Map'){
      message('Getting pitch types')
      
      if(input$pitch_type == 'All'){
        filter_pitch=NULL
      }else{
        filter_pitch=input$pitch_type
      }
      
      output$plot <- renderPlot({player_pitcher$create_pitch_scatter(filter_pitch)})
      
      shinyjs::show("plot")
      
    }
    
    
    if(input$viz == 'Pitch Time Series'){
      
      filter_pitch=input$pitch_type
      
      pitch_measure <- switch(input$pitch_measure,
                              "Spin Rate"="release_spin_rate",
                              "Pitch Speed"="release_speed")
      
      output$plot <- 
        renderPlot({player_pitcher$create_avg_pitch_chart(filter_pitch=filter_pitch, 
                                                          values = pitch_measure)})
      shinyjs::show("plot")
      
    }
    
    
    if(input$viz == 'Batting Times Series'){
      
      message('Making timeseries for ', input$stat_type)
      
      stat_map <- list(WOBA = 'woba_value',
                       ISO = 'iso_value',
                       "Exit Velocity" = 'launch_speed',
                       "Launch Angle" = 'launch_angle',
                       "Estimated WOBA" = 'estimated_woba_using_speedangle')
      
      hit_measure <- switch(input$stat_type,
                            "WOBA" = 'woba_value',
                            "ISO" = 'iso_value',
                            "Exit Velocity" = 'launch_speed',
                            "Launch Angle" = 'launch_angle',
                            "Estimated WOBA" = 'estimated_woba_using_speedangle')
      
      message(input$p_hand)
      message(input$pitch_type)
      
      hand_filter <- ifelse(input$p_hand == 'Both', c('L','R'), input$p_hand)
      pitch_filter <- ifelse(input$pitch_type == 'All', 'all', input$pitch_type)
      
      message(hand_filter)
      message(pitch_filter)
      
      
      output$plot <- 
        renderPlot({ player_hitter$generate_outcome_timeseries(stat = trimws(hit_measure,'both'),
                                                               p_hand = hand_filter,
                                                               pitch = pitch_filter)})
      shinyjs::show("plot")
      
    }
    
    if(input$viz == 'Spray Chart'){
      
      message(input$multi_pitch_types)
      message(input$outcomes)
      message(input$p_hand_multi)
      message(input$group)
      
      group_map <- switch(input$group,
                            "Pitcher Throws" = 'p_throws',
                            "Outcome" = 'events',
                            "Pitch Type" = 'pitch_name',
                            "None" = "None")
      
      message(group_map)
      
      output$plot <- 
        renderPlot(
            {
              player_hitter$generate_spary_chart(bucket = group_map, 
                                         filters = list(events = input$outcomes,
                                                        p_throws = input$p_hand_multi,
                                                        pitch_name = input$multi_pitch_types))
              }
            )
      
      shinyjs::show("plot") 
    }
    
  })
  
}