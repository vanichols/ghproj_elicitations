#--just getting a ghetto example to put into a one-pager

library(ggplot2)
library(PESTO)
#library(PesticideLoadIndex4Dummies) #--need to test this more...

tst <- pesto_exinput

p <- MakeImage4R()

p

ggsave("figs/fig_example-fairy-dust.png", width = 9, height = 9)
