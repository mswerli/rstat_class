
ui <- fluidPage(
  
  useShinyjs(),
  
  ##Download data tab
  tabsetPanel(id = 'tabs',
              tabPanel("Select a Player", fluid = TRUE,
                       column( width = 4,
                               sidebarLayout(
                                 sidebarPanel(width = 12, id="sidebar",
                                              h5("Pick a Player Season"),
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
                                                          if_else(month(today()) < 6, 
                                                                  as.numeric(year(today())) - 1, 
                                                                  year(today()))),
                                              
                                              actionButton("get_data",
                                                           "get_data")
                                 ),
                                 mainPanel( width = 0)
                               )
                       )
              ),
              
              ##Visualization Tab
              tabPanel("Visualizations", fluid = TRUE,
                       column(width = 4,
                              sidebarLayout(
                                sidebarPanel(width = 12, id="sidebar",
                                             h5("Visualization Options"),
                                             
                                             selectInput('viz', "Visualization", 
                                                         c('Pitch Mix', 
                                                           'Pitch Heat Map', 
                                                           "Pitch Time Series", 
                                                           "Spray Chart", 
                                                           'Batting Times Series'),
                                                         ""),
                                             
                                             selectInput("pitch_type", "Pitch Type", '',''),
                                             
                                             selectInput("pitch_measure", "Measurement", 
                                                         c('Pitch Speed', 'Spin Rate'),''),
                                             
                                             selectInput("stat_type", "Hitting Statistic", 
                                                         c('WOBA','ISO','Exit Velocity',
                                                           'Launch Angle','Estimated WOBA'),
                                                         'WOBA'),
                                             
                                             
                                             selectInput("p_hand", "Pitcher Throws",
                                                         c('L','R','Both'),'Both'),
                                             
                                             selectInput("group", "Grouping",
                                                         c('None','Pitcher Throws', 
                                                           'Outcome', 'Pitch Type'),
                                                         'None'),
                                             
                                             
                                             actionButton("gen_viz",
                                                          "Generate Viz")
                                             ),
                                
                                mainPanel( width = 0)
                                ),
                              sidebarLayout(
                                sidebarPanel(width =12, id="filters",
                                             h5("Spray Chart Filters"),
                                             
                                             selectInput("outcomes", 
                                                         "outcomes",'','', 
                                                         multiple = TRUE, 
                                                         selectize = TRUE),
                                             
                                             selectInput("multi_pitch_types", 
                                                         "Pitch Types",'','', 
                                                         multiple = TRUE, 
                                                         selectize = TRUE),
                                             
                                             selectInput("p_hand_multi", 
                                                         "Pitcher Throws ", 
                                                         c('L','R'),
                                                         c('L','R'),
                                                         multiple = TRUE,
                                                         selectize = TRUE)
                                ),
                                mainPanel( width = 0)
                                
                              )
                       ),
                       mainPanel(plotOutput("plot"))
              )
)
  )