
library(ggplot2)
library(dplyr)
library(datasets)
method_a <- function(){
data(iris)
head(iris)
summary(iris)
df <- as.data.frame(iris)
print(df %>% filter(Sepal.Length > 5))
print("--------------")
print(df %>% filter(Species == "virginica"))
print("--------------")
print(df %>% filter(Petal.Length > 5))
print("--------------")
} 
method_b <- function(){
  df <- as.data.frame(iris)
  Regtangularity <- df %>% transmute(abs_diff = abs(Sepal.Width - Sepal.Length)) %>% pull(abs_diff)
  print(Regtangularity)
}
method_c <- function(){
  df <- as.data.frame(iris)
  Regtangularity <- df %>% transmute(abs_diff = abs(Sepal.Width - Sepal.Length)) %>% pull(abs_diff)
  filtered_iris <- df %>% mutate(Sepal.Rectangularity = Regtangularity)
  print(filtered_iris)
}
method_d_e_f <- function(){
  df <- as.data.frame(iris)
  Regtangularity <- df %>% transmute(abs_diff = abs(Sepal.Width - Sepal.Length)) %>% pull(abs_diff)
  filtered_iris <- df %>% mutate(Sepal.Rectangularity = Regtangularity)
  newone <- filtered_iris %>% arrange(desc(Sepal.Rectangularity))
  head(newone,7)
  X <- rowMeans(cbind(filtered_iris$sepal.rectangularity, filtered_iris$Petal.Length, 
          filtered_iris$Petal.Width)) > 5
  newtwo <- filtered_iris %>% mutate(X = X)
  print(newtwo)
  iris_matrix <- as.matrix(newtwo)
  head(iris_matrix)
}


