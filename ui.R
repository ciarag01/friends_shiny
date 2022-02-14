#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel(tags$h1("Friends")),
  
  navbarPage("Navigation Page",
             
             tabPanel("Which character speaks most?", icon = icon("comments"),
                      
                      fluidRow(column(3,   
                                      selectInput("season", "Select Season", 
                                                       choices = unique(speakers$season), 
                                                       multiple = TRUE
                      ))),
   
                        fluidRow(
                          column(6,
                                 uiOutput("ui_plot"))
                      )
                      ), 
             
             tabPanel("Emotions of lines", icon = icon("heart"), 
                      fluidRow(
                        column(12, 
                               selectInput("season_emotion", "Select Season", 
                                           choices = unique(emotions$season)))
                      ),
                      fluidRow(
                        column(12, 
                               plotOutput("emotion_chart"))
                      )),
             
             tabPanel("Show Ratings", icon = icon("star"),
                      fluidRow(
                        column(3, 
                               selectInput("season_rating", "Select Season", 
                                           choices = unique(ratings$season), 
                                           multiple = TRUE)),
                        column(3, 
                               selectInput("rating_measure", "Ratings or Views", 
                                           choices = unique(ratings$rating_measure)))
                      ), 
                      fluidRow(
                        column(6, 
                               plotOutput("ratings_chart")), 
                        
                        column(6, 
                               plotOutput("char_ratings_chart"))
                      )),
             
             tabPanel("Word Frequency", icon = icon("cloud"), 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("season_words", "Select Season", 
                                      choices = unique(words$season)
                                      ), 
                          selectInput("character_words", "Select Character", 
                                      choices = unique(words$speaker_grpd)
                          ),
                          sliderInput("freq",
                                      "Min Word Frequency:",
                                      min = 1,  max = 50, value = 15),
                          sliderInput("max",
                                      "Max Number of Words to Show:",
                                      min = 1,  max = 300,  value = 100)
                        ), 
                        mainPanel(
                          plotOutput("word_cloud")
                        )
                      ))
                      )
  
))
