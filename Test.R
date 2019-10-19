#Вариант №24
df <- mtcars
library(dplyr)
#Задание 1

df_4_cyl <- df %>% filter(cyl == 4) %>%  select(mpg)
df_8_cyl <- df %>% filter(cyl == 8)  %>%  select(mpg)

res<- t.test(df_4_cyl, df_8_cyl)
# Использовался t.test из базового пакета R. Для сравнения 2х выборок. Он показал, что значения потребляемого топлива, различаются. (точнее что вероятность получить такие или ещё более сильные различия очень мала)
library(ggplot2)
ggplot (df, aes(x = factor(cyl), y = mpg))+
geom_boxplot()
# На графике можно посмотреть, что потребления топлива у 4х цилиндровых автомобилей намного выше, что согласуется с данными t-test
#
#
#Задание 2
df_4_cyl <- df %>% filter(cyl == 4) %>%  select(hp)
df_6_cyl <- df %>% filter(cyl == 6) %>%  select(hp)
df_8_cyl <- df %>% filter(cyl == 8)  %>%  select(hp)

cyl_4 <- median(df_4_cyl$hp)
cyl_6 <- median(df_6_cyl$hp)
cyl_8 <- median(df_8_cyl$hp)

result <- data_frame(cyl_4, cyl_6, cyl_8)

# Самый высокий показаатель имеют ммашины с 8 цилиндрами (192 лошадиных силы)

