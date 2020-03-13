#Recomendation system
library("recommenderlab")
library(caTools)
books <- read.csv(file.choose())
books <- books[-1]
#metadata about the variable
str(books)
hist(books$ratings...3.)

#Popularity based 
#the datatype should be realRatingMatrix inorder to build recommendation engine
booksmatrix <- as(books, 'realRatingMatrix')


booksmodel1 <- Recommender(booksmatrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(booksmodel1, booksmatrix[10:11], n=5)
as(recommended_items1, "list")


#User Based Collaborative Filtering

movie_recomm_model2 <- Recommender(booksmatrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(movie_recomm_model2, booksmatrix[413:414], n=5)
as(recommended_items2, "list")






