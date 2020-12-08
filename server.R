## server.R

library(dplyr)

# load functions
source('functions/cf_algorithm.R') # collaborative filtering



# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

Rmat = createRatingMatrix(ratings)
rec = getUBCFRecommender(Rmat)

shinyServer(function(input, output, session) {
  
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 100
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  output$genres_dropdown <- renderUI({
    
    genres = sort(unique(unlist(strsplit(movies$Genres,'[|]'))))
    selectInput("genres_select", 
                "",
                choices = genres
    )
  })  
  # Calculate recommendations when the sbumbutton is clicked
  rdf <- eventReactive(input$rbtn, {
    withBusyIndicatorServer("rbtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      print(user_ratings)
      
      predictRecom(user_ratings,Rmat,rec,movies)
      
    }) # still busy
    
  }) # clicked on button
  
  # Calculate recommendations when the sbumbutton is clicked
  gdf <- eventReactive(input$gbtn, {
    withBusyIndicatorServer("gbtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      print(input$genres_select)
      moviesByGenre =  movies %>% 
        filter( grepl(input$genres_select,Genres))
      tmp = ratings %>% 
        group_by(MovieID) %>% 
        summarize(ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig=3)) %>%
        inner_join(moviesByGenre, by = 'MovieID')
      
      recom_results = tmp %>%
        filter(ratings_per_movie > mean(tmp$ratings_per_movie) ) %>%
        top_n(10, ave_ratings) %>%
        arrange(desc(ave_ratings)) %>%
      select('MovieID','Title', 'ave_ratings','ratings_per_movie') 
 
  
    }) # still busy
    
  }) # clicked on button
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- rdf()
    print(recom_result)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j],]$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  output$resultsByGenre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    grecom_result <- gdf()
    print(grecom_result)
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies[movies$MovieID == grecom_result$MovieID[(i - 1) * num_movies + j],]$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(grecom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function