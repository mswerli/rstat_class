baseball_field <- R6::R6Class(
  'baseball_field',
  public = list(
    
    strike_zone = NA,
    zone_axes = NA,
    zone = NA,
    x_limit = NA,
    y_limit = NA,
    right_line= NA,
    left_line = NA,
    fence = NA,
    infield = NA,
    spray_chart_base=NA,
    
    initialize  = function(){
      
      self$x_limit <<- xlim(0,250)
      self$y_limit <<- ylim(-250, 0)
      self$right_line <<- geom_segment(x=128, xend = 33, y=-208, yend = -100)
      self$left_line <<- geom_segment(x=128, xend = 223, y=-208, yend = -100)
      self$fence <<- geom_curve(x = 83, xend = 173, y = -155, yend = -156,
                                curvature = -.65, linetype = "dotted")
      self$infield <<- geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                                  curvature = -.65)
      
      self$spray_chart_base <<-
        ggplot(self$data, aes(x = as.numeric(hc_x), y = -as.numeric(hc_y))) +
        geom_point(fill = "blue",
                   color = "grey20", alpha = .75,
                   shape = 21, size = 1, stroke = 1)+
        self$x_limit+
        self$y_limit+
        self$infield+
        self$fence+
        self$right_line +
        self$left_line
      
      self$strike_zone <<-
        geom_rect(
          xmin = -.85,
          xmax = .85,
          ymin = 1.6,
          ymax = 3.4,
          color = 'Black',
          fill = NA
        )
      
      self$zone_axes <<-
        list(x = scale_x_continuous(
          limits = c(-1.5, 1.5),
          breaks = seq(-1.5, 1.5, .5)
        ),
        y = scale_y_continuous(limits = c(1, 4),
                               breaks = seq(1, 4, .5)))
      
    }
  )
)