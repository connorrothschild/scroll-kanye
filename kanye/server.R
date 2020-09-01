library(shiny)
library(highcharter)

# load(url('https://github.com/scottyd22/dream_team/blob/master/dream_team_data.RData?raw=true'))

source('utils.R')

data(diamonds, economics_long, mpg, package = "ggplot2")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # type ----
    # Reactive Value to track scrolly sections
    type.rx <- reactiveValues(d = NULL)
    
    # Update the reactive value as users scroll through site
    observeEvent(input$type, {
        if(input$type %in% type.rx$d) {
            type.rx$d <<- type.rx$d[1:match(input$type, type.rx$d)]
        } else if(input$type == '1992') {
            type.rx$d <<- type.rx$d
        } else {
            type.rx$d <<- unique(c(type.rx$d, input$type)) 
        }
    })
    
    
    # Metric plot----
    # Radio buttons to select metric to plot
    output$metric_list <- renderUI({
        radioGroupButtons(
            inputId = "metric", label = "",
            choices = c('Points', 'Rebounds', 'Assists', 'Steals', 'Blocks', 'FG%', '3P%', 'FT%', 'Minutes'),
            selected = 'Points',
            size = 'sm',
            justified = TRUE,
            status = "default"
        )
    })
    
    # Plot of career stats/metrics
    output$metrics <- renderHighchart({
        
        # When users scroll to the section "1992", change the chart
        if(input$type == 'all') {
            data <- mpg
            hc <- hchart(data, "scatter", hcaes(x = cyl, y = hwy, group = class))
        } else {
            data <- mpg %>% filter(class == input$type)
            hc <- hchart(data, "scatter", hcaes(x = displ, y = hwy, group = class))
        }
        
            hc
        
    })
    
    # Box Scores ----
    output$plot <- renderHighchart({
       
        mpgman2 <- mpg %>% 
            count(class, year)
        
        # Plot
        hc <- hchart(mpgman2, "column", hcaes(x = class, y = n, group = year))
        
        hc
        
    })
    
    # Scrollytell
    output$type <- renderScrollytell({scrollytell()}) 
    observe({cat("section:", input$scr, "\n")})
    
    # Update restart.txt to clear cache
    session$onSessionEnded(function() {
        restart = data.frame(`modified time` = lubridate::now())
        write.table(restart, file = 'restart.txt', row.names = F)
    })

}
