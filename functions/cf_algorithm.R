
# ============================================================
# Functions used in implementation of collaborative filtering.
# ============================================================


library(recommenderlab)

createRatingMatrix = function(train){
  i = paste0('u', train$UserID)
  j = paste0('m', train$MovieID)
  x = train$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  dim(Rmat)
  new('realRatingMatrix', data = Rmat)  
}


getUBCFRecommender = function(Rmat) {
  Recommender(Rmat, method = 'UBCF',
              parameter = list(normalize = 'Z-score', 
                               method = 'Cosine', 
                               nn = 25))
}

getIBCFRecommender = function(Rmat) {
  Recommender(Rmat, method = 'IBCF',
              parameter = list(normalize = 'Z-score', 
                               method = 'Cosine', 
                               k = 30))
}

predictRecom = function(user_ratings, movieCols, rec) {
  i = rep('u0', length(movieCols))
  j = movieCols
  x = rep(0, length(movieCols))
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  URmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  colnames(URmat) = movieCols
  for (u in 1:nrow(user_ratings)){
    colName = paste0('m',user_ratings[u,]$MovieID)
    if (is.element(colName, movieCols)) { #rated movie
      URmat[1,colName] = user_ratings[u,]$Rating
    }
  }
  URmat = new('realRatingMatrix', data = URmat)  
  recom = predict(rec, 
                  URmat, type = 'ratings')  
  rec_list = as(recom, 'list')
  #print(rec_list)
  selected = names(tail(sort(rec_list[[1]]),10))
  ids = as.integer(substring(selected,2))
  ids
}