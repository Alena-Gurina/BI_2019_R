#We use UTF-8 coding
# пользовательские функции вычисления корреляции
# 
my_cor_test<- function(df){
    x = df[,1]
    y = df[,2]
    cov_coef = sum((x-mean(x))*(y-mean(y)))/(length(x)-1)
    res_cor = cov_coef/((((sum((x-mean(x))^2))/((length(x)-1))*((sum((y-mean(y))^2)))/((length(y)-1))))^0.5)
  return (res_cor)}


x <- c(1,1,4,2,6,4,5,10,15,7,5)
y <- c(1,3,3,7,5,5,2,14,13,3,8)

df <- data.frame(x, y)
my_cor_test(df)
test_res <- cor.test(df$x, df$y)
test_res$estimate

library(dplyr)
library(data.table)

my_cor_spir <- function(df){
  x = df[,1]
  y = df[,2]
  new_df = data.frame(x = factor(x), y = factor(y))
  n = nrow(new_df)
  new_df$numb <- c(1:n)
  new_df<- new_df %>% group_by(x) %>% mutate(rang_x = mean(numb))
  new_df<- new_df %>% group_by(y) %>% mutate(rang_y = mean(numb))
  new_df<- new_df %>% mutate(diff = (rang_x-rang_y)^2)
  popr_x <- (setDT(new_df)[, .(Count = .N) , by = rang_x])$Count
  popr_y <- (setDT(new_df)[, .(Count = .N) , by = rang_y])$Count
  d_x = (1/12)*(sum(popr_x^3 - popr_x))
  d_y = (1/12)*(sum(popr_y^3 - popr_y))
  coef_cor = 1 - ((sum(6*(new_df$diff))+d_x+d_y)/(n^3 - n))
  return (coef_cor)
}


my_cor_spir(df)

cor.test(x = df$x, y = df$y , method = "spearman")

