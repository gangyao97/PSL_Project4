# load functions
#source('functions/ubcf.R') # UBCF algorithm
library("plyr")                      # Load plyr package
library(dplyr)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
get_user_ratings = function(value_list,num_col) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  # get the indices of the ratings
  user_ratings <-  matrix(0, nrow = 1, ncol =num_col )
 
  for (i in 1:nrow(dat)){
    user_ratings[dat[i]$MovieID]=dat[i]$Rating
  }
  return (user_ratings)

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

# reshape to movie x user matrix 
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
#test2 =Rmat[501, ]
Rmat = new('realRatingMatrix', data = Rmat)

train = Rmat[1:500, ]


shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
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
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
     
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list,ncol(Rmat))
      
    
      colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
      i = paste0('u', ratings$UserID)
      j = paste0('m', ratings$MovieID)
      x = ratings$Rating
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      Rmat1 = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(Rmat1) = levels(tmp$i)
      colnames(Rmat1) = levels(tmp$j)
     
      NewMatrix <- rbind(Rmat1, user_ratings)
     
    #  user_ratings = t(user_ratings)
      NewMatrix = new('realRatingMatrix', data = NewMatrix)
      test2 = NewMatrix[6041,]
      
  
      recommender.UBCF <- Recommender(train, method = 'UBCF',
                                      parameter = list(normalize = 'Z-score', 
                                                       method = 'Cosine', 
                                                       nn = 20))
     
      
      p.UBCF <- predict(recommender.UBCF, test2, type="ratings")
      p.UBCF <- as.numeric(as(p.UBCF, "matrix"))
      
   
      # sort, organize, and return the results
      user_results <- sort(p.UBCF, decreasing = TRUE,index.return=TRUE)[1:20]
 
      user_predicted_ids <- user_results$ix
     
      recom_results <- data.table(Rank = 1:20, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
   
      
    }) # still busy
    
  }) # clicked on button
  
  
  # Calculate recommendations when the sbumbutton is clicked for system 1
  df1 <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      print("system1 recomemmendation")
      select_result<- input$Genre
 
      print(select_result)
     
      genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
      tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                    type.convert=TRUE),
                          stringsAsFactors=FALSE)
      genre_list = c("Action", "Adventure", "Animation", 
                     "Children's", "Comedy", "Crime",
                     "Documentary", "Drama", "Fantasy",
                     "Film-Noir", "Horror", "Musical", 
                     "Mystery", "Romance", "Sci-Fi", 
                     "Thriller", "War", "Western")
      m = length(genre_list)
      genre_df = matrix(0, nrow(movies), length(genre_list))
      for(i in 1:nrow(tmp)){
        genre_df[i,genre_list %in% tmp[i,]]=1
      }
      colnames(genre_df) = genre_list
      remove("tmp", "genres")
      # get the user's rating data

      moive_results <- ratings %>% 
        left_join(data.frame(MovieID = movies$MovieID, genre_df), 
                  by = "MovieID") %>%
        filter(get(select_result) == 1) %>%
        group_by(MovieID) %>% 
        summarize(ratings_per_movie = n(), 
                  ave_ratings = round(mean(Rating), dig=3)) %>%
        inner_join(movies, by = 'MovieID') %>%
        filter(ratings_per_movie > 1000) %>%
        #filter(Action) %>%
        top_n(10, ave_ratings) 
      
      user_results = moive_results$ave_ratings
      user_predicted_ids = moive_results$MovieID
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  output$select_result <- renderText({
    paste("You chose", input$Genre)
  })
  
  
  
  # display the recommendations for system 2
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # display the recommendations for system 1
  output$results.Genre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df1()
  # modify code below to fit the result
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
}) # server function