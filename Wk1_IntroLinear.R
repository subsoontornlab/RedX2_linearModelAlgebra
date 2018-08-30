library(UsingR)
library(dplyr)
data("father.son",package="UsingR")
head(father.son)

#avg height of son
mean(father.son$sheight)

# son whose father height is round to 21
s_f71 = filter(father.son, round(fheight) == 71) %>% select(sheight) %>% unlist()
mean(s_f71)