## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home",tabName = "Home"),
        menuItem("System 1 - Genre",tabName = "System1"),
        menuItem("System 2 - Collaborative", tabName = "System2")
      )
    ),

    #dashboardSidebar(disable = TRUE),
    dashboardBody(
      includeCSS("css/movies.css"),
      tabItems(
        # Home page content
        tabItem(tabName = "Home",
        fluidRow(
          box(width = 12, title = "Choose a reccomendation system", status = "info", solidHeader = TRUE,
              "System 1: reccomendation by genre",br(),br(),"System 2: reccomendation by your rating choice",br()
              )
          )
        ),
        # System1 tab content
        tabItem(tabName = "System1",
                fluidRow(
                   box(width = 12,height  = 150,  title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, 
                      selectInput("Genre", "Select a genre", c("Action", "Adventure", "Animation", 
                                                               "Children.s", "Comedy", "Crime",
                                                                "Drama", "Fantasy",
                                                               "Film.Noir", "Horror", "Musical", 
                                                               "Mystery", "Romance", "Sci.Fi", 
                                                               "Thriller", "War", "Western")),
                  ),
                ),
                fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results.Genre")
                  )
                )
        ),

        # System2 tab content
        tabItem(tabName = "System2",
                fluidRow(
                  box(width = 12, title = "Step 1: Rate as many Movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings'),
                      ),
                  )
                ),
                fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
                )
        )
      )
    )
  )
) 