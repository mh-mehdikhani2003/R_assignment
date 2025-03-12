library(ggplot2)
library(dplyr)
library(datasets)
method <- function(){
  df <- read.csv("data.csv")
  head(df)
  subset_df <- subset(df, sweet > 2.5)
  col_vector <- subset_df$col
  if ("poineer" %in% names(df)) {
    print("Yes")
  } else {
    print("No")
  }
}
random_shuffle <- function(df){
  shuffled_df <- df[sample(nrow(df)), ]
  return(shuffled_df)
}
get_top_rows_variance <- function(df,n,col){
  top_n_variance <- var(sort(df$col, decreasing = TRUE)[1:n])
  return(top_n_variance)
}
main <- function(){
 data(mtcars)
  head(mtcars)
  summary(mtcars)
  cars <- as.data.frame(mtcars)
  cars$cyl <- sapply(cars$cyl, function(x) x * 2)
  print(cars)
  print("--------------")
  medians <- aggregate(. ~ 1, data = cars, FUN = median)
  print(medians) 
  vec <- 1:100
  print("--------")
  for (i in 1:100) {
    cars <- random_shuffle(cars)
    vec[i] <- get_top_rows_variance(cars,i,disp)
  }
}