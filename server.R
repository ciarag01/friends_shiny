#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ##### LINES PER CHARACTER #####
  output$ui_plot <- renderUI({
    out <- list()
    if (length(input$season)==0){return(NULL)}
    for (i in 1:length(input$season)){
      out[[i]] <-  plotOutput(outputId = paste0("plot",i))
    }  
    return(out) 
  })

  observe({  
    for (i in 1:3){  
      local({  #because expressions are evaluated at app init
        ii <- i 
        output[[paste0('plot',ii)]] <- renderPlot({ 
          if ( length(input$season) > ii-1 ){  
            return(fn2(season = input$season[[ii]]))
          } 
          NULL
        })
      })
    }                                  
    
  })


    # output$lines_chart <- renderPlot({
    #   ggplot(plot_data_lines()) +
    #     geom_col(aes(x = speaker_grpd, y = tot, fill = speaker_grpd), show.legend = FALSE)+
    #     geom_text(aes(x = speaker_grpd, y = tot, label = speaker_grpd),
    #               size = 12,
    #               colour = "grey",
    #               family = "JetBrains Mono") +
    #     scale_fill_viridis_d()+
    #     theme_minimal() +
    #     theme(axis.ticks.x = element_blank(),
    #           axis.text.x = element_blank()) +
    #     coord_cartesian(ylim = c(0, 1092)) +
    #     labs(x = " ", y = "Number of Lines")

  ##### EMOTIONS OF LINES #####
    
  plot_data_emotions <- reactive({
    emotions %>% 
      filter(season %in% input$season_emotion)
  })
  
  output$emotion_chart <- renderPlot({
    ggplot(plot_data_emotions(), aes(x = speaker_grpd, y = perc, fill = emotion)) +
      geom_bar(stat = "identity")
  })
  
  ##### RATINGS OF EPISODES ######
  
  plot_data_ratings <- reactive({
    ratings %>% 
      filter(season %in% input$season_rating) %>% 
      filter(rating_measure == input$rating_measure)
  })
  
  output$ratings_chart <- renderPlot({
    ggplot(plot_data_ratings()) +
      geom_line(aes(x = episode, y = n, colour = season)) +
      labs(y = input$rating_measure) +
      ylim(0, max(plot_data_ratings()$n))
  })
  
  output$char_ratings_chart <- renderPlot({
    ggplot(plot_data_ratings(), aes(x = episode, y = n, fill = main_char)) +
      geom_bar(stat = "identity", position = "dodge")
  })
  
  ##### WORD CLOUD #####
  
})
