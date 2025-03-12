generate_chord_way1 <- function(radius = 1) {
  
  theta1 <- runif(1, 0, 2 * pi)
  theta2 <- runif(1, 0, 2 * pi)
  
  x1 <- radius * cos(theta1)
  y1 <- radius * sin(theta1)
  x2 <- radius * cos(theta2)
  y2 <- radius * sin(theta2)
  
  chord_length <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(chord_length)
}
main <- function(way){
  count_true <- 0
  count_false <- 0  
  if(way == 1){
    set.seed(42)
    chord_lengths_way1 <- replicate(1000, generate_chord_way1())
  
    for (i in 1:1000) {
      if(chord_lengths_way1[i] > sqrt(3)){
        count_true <- count_true +1
      }else{
        count_fasle <- count_false +1
      }
    }
    print(count_true/count_false)
  }
  if(way == 2){
    random_numbers_5_to_10 <- runif(1000, min = 0, max = 1)
    for (i in 1:1000) {
      if(sqrt(1-(random_numbers_5_to_10[i])^2)*2 > sqrt(3)){
        count_true <- count_true +1
      }else{
        count_fasle <- count_false +1
      }
    }
    print(count_true/count_false)
  }
  if(way == 3){
    random_numbers_0_to_1 <- runif(1000, min = 0, max = 1)
    for (i in 1:1000) {
      if(sqrt(1-(random_numbers_5_to_10[i])^2)*2 > sqrt(3)){
        count_true <- count_true +1
      }else{
        count_fasle <- count_false +1
      }
    }
    print(count_true/count_false)
  }
}