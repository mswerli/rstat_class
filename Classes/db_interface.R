db_interface <- R6::R6Class(
  'db_interface',
  private = list(
    
    db_creds = list(
      username = Sys.getenv('db_user'),
      password = Sys.getenv('password'),
      host = Sys.getenv('db_host'),
      port = Sys.getenv('db_port'),
      database = Sys.getenv('database'),
      sslmode = require
    )
    
    
  ),
  public = list(
    db_conn=NA,
    
    initialize = function() {
      message('Setting parameters for request')
      
      self$db_conn <<- dbConnect(RPostgres::Postgres(),
                                 dbname = private$db_creds$database, 
                                 host =  private$db_creds$host,
                                 port =  private$db_creds$port,
                                 user =  private$db_creds$username,
                                 password =  private$db_creds$password)
      
    },
    
    query = function(schema, table, columns=NA, filter=NA, render_sql = FALSE){
      
      data <- dplyr::tbl(con, dbplyr::in_schema(schema,table)) 
      
      if(!is.na(columns)){
        
        data <- data %>% select(!!(columns))
        
      }
      
      return(data)
      
      
    }
    
  )
)