library(tidyverse)
library(modelr)
options(na.action = na.warn)

diamonds

ggplot(diamonds,aes(cut,price))+geom_boxplot()
ggplot(diamonds,aes(color,price))+geom_boxplot()
ggplot(diamonds,aes(clarity,price))+geom_boxplot()

ggplot(diamonds,aes(carat,price))+geom_hex(bins=50)

diamonds3 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice=log2(price),lcarat=log2(carat))
diamonds3

ggplot(diamonds3,aes(lcarat,lprice))+geom_hex(bins=50)

mod_diamond <- lm(lprice~lcarat,data=diamonds3)
mod_diamond

model_matrix(diamonds3,lprice~lcarat)

#grid <- diamonds3 %>%
 # data_grid(carat=seq_range(carat,20))
#grid

#grid <- diamonds3 %>%
 # data_grid(carat=seq_range(carat,20)) %>%
  #mutate(lcarat=log2(carat))
#grid


#grid <- diamonds3 %>%
 # data_grid(carat=seq_range(carat,20)) %>%
  #mutate(lcarat=log2(carat)) %>%
  #add_predictions(mod_diamond,"lprice")
#grid


grid <- diamonds3 %>%
  data_grid(carat=seq_range(carat,20)) %>%
  mutate(lcarat=log2(carat)) %>%
  add_predictions(mod_diamond,"lprice") %>%
  mutate(price=2^lprice)
grid

ggplot(diamonds3,aes(carat,price))+geom_hex(bins=50)+geom_line(data=grid,
                                                      color="red",size=1)
#diamonds3$carat
#grid$carat


diamonds3
diamonds3_1 <- diamonds3 %>%
  add_residuals(mod_diamond,"lresid")
diamonds3_1

#residual dnt contain impact of carat on price
ggplot(diamonds3_1,aes(cut,lresid))+geom_boxplot()
ggplot(diamonds3_1,aes(color,lresid))+geom_boxplot()
ggplot(diamonds3_1,aes(clarity,lresid))+geom_boxplot()

mod_diamond2 <- lm(lprice~lcarat+color+cut+clarity,data=diamonds3)
mod_diamond2

model_matrix(diamonds3,lprice~lcarat+color+cut+clarity)
#grid <- diamonds3 %>%
 # data_grid(cut,.model=mod_diamond2)
#grid


grid <- diamonds3 %>%
  data_grid(cut,.model=mod_diamond2) %>%
  add_predictions(mod_diamond2,"price")
grid

ggplot(grid,aes(cut,price))+geom_point()

