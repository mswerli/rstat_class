source('src/init.R')

teams <- jsonlite::read_json('config/team_ref.json', 
                             simplifyVector = TRUE)

ui <- fluidPage(
  
  useShinyjs(),
  
  ##Download data tab
  tabsetPanel(
    tabPanel("Select a Player", fluid = TRUE,
             
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
                         if_else(month(today()) < 4, 
                                 as.numeric(year(today())) - 1, 
                                 year(today()))),
             
             actionButton("get_data",
                          "get_data")
    ),
    
    ##Pitcher Visualization Tab
    tabPanel("Pitching Visualizations", fluid = TRUE,
             
             selectInput('viz', "Visualization", 
                         c('Pitch Mix', 'Pitch Heat Map', "Pitch Time Series"),
                         ""),
             
             selectInput("pitch_type", "Pitch Type", '',''),
             
             selectInput("pitch_measure", "Measurement", 
                         c('Pitch Speed', 'Spin Rate'),''),
             
             actionButton("gen_viz",
                          "Generate Viz"),
             
             
             mainPanel(plotOutput("plot"))
    )
  )
  
)

server <- function(input, output,session) {
  
  current_viz <- NA
  
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
    
    if(input$Position == 'Pitchers'){
      
      message('Getting pitching data ', input$team)
      
      ## Create instnace of pitcher class based on inputs
      player_pitcher <<- pitcher_instance$new(
        player_type = 'pitcher',
        player_name=input$Player,
        batters_lookup = NULL,
        pitchers_lookup=get_roster(input$Team, teams, input$Season) %>%
          filter(name_display_first_last == input$Player) %>%
          select(player_id) %>% unlist() %>%
          as.character(),
        team=input$Tean,
        season=input$Season)
      
      ############################
      ##### Start of Viz Tab #####
      ############################
      # Single use for all positions 
      
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
      shinyjs::showElement(id='pitch_measure')
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
  })
  
  ##Actions when gen_viz button is clicked
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
  })
    
  }
  
  shinyApp(ui = ui, server = server)
  
  