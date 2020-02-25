source('src/init.R')
source('shiny_app/ui.R')
source('shiny_app/server.R')


teams <- jsonlite::read_json('config/team_ref.json', 
                             simplifyVector = TRUE)

shinyApp(ui = ui, server = server)
  
  