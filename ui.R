## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

rating.tab = tabItem(tabName = "ratingsTab",
                     h2("Recommender by Ratings"),
                     fluidRow(
                       box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                           div(class = "rateitems",
                               uiOutput('ratings')
                           )
                       )
                     ),
                     fluidRow(
                       useShinyjs(),
                       box(
                         width = 12, status = "info", solidHeader = TRUE,
                         title = "Step 2: Discover movies you might like",
                         br(),
                         withBusyIndicatorUI(
                           actionButton("rbtn", "Click here to get your recommendations", class = "btn-warning")
                         ),
                         br(),
                         tableOutput("results")
                       )
                     )
)
genre.tab = tabItem(tabName = "genreTab",
                     h2("Recommender by Genre"),
                    fluidRow(
                      box(width = 12, title = "Step 1: Select Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          uiOutput("genres_dropdown"),
                      )
                    ),
                    fluidRow(
                       useShinyjs(),
                       box(
                         width = 12, status = "info", solidHeader = TRUE,
                         title = "Step 2: Discover movies you might like",
                         br(),
                         withBusyIndicatorUI(
                           actionButton("gbtn", "Click here to get your recommendations", class = "btn-warning")
                         ),
                         br(),
                         tableOutput("resultsByGenre")
                       )
                     )
)
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movies Recommender"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("System I", tabName = "genreTab", icon = icon("film")),
        menuItem("System II", tabName = "ratingsTab", icon = icon("star"))
       )
    ),
    
    dashboardBody(includeCSS("css/movies.css"),
                  tabItems(genre.tab, rating.tab
                           
                  ),
    )
  )
) 