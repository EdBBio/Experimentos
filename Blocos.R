library(tidyverse)
library(readxl)
library(ggforce)

blocos<-as.data.frame(read_xlsx("G:/Meu Drive/R/Experimentos/blocos.xlsx"))
blocos

blocos %>% 
  ggplot(aes(`média segmentos/blocos`,`Valores`))+
  geom_point(color="black",
             size=2.5)+
  labs(x="média dos segmentos/blocos",
       y="scores",
       color="tipo de tratamento",
       fill="tipo de tratamento")+
  geom_mark_ellipse(aes(color=`Preseça/ausência de som`,
                        fill=`Preseça/ausência de som`))+
  theme(title=element_text(color="black"),
        axis.text=element_text(color="black"),
        axis.line=element_line(color="black"),
        panel.grid=element_line(color="gray"),
        panel.background=element_rect(fill="white"),
        legend.key=element_rect(fill="white"),
        plot.background=element_rect(fill="white",color="white"))

testetblocos<-data.frame(valores1=blocos[1:10,1],valores2=blocos[11:20,1])

shapiro.test(testetblocos$valores1)
shapiro.test(testetblocos$valores2)

var.test(testetblocos$valores1,testetblocos$valores2)

t.test(testetblocos$valores1,testetblocos$valores2,var.equal = TRUE,alternative ="two.sided")

blocos %>% 
  ggplot(aes(`Preseça/ausência de som`,`Valores`,
             fill=`Preseça/ausência de som`))+
  geom_boxplot(color="black")+
  theme(title=element_text(color="black"),
      axis.text=element_text(color="black"),
      axis.line=element_line(color="black"),
      panel.grid=element_line(color="gray"),
      panel.background=element_rect(fill="white"),
      legend.key=element_rect(fill="white"),
      plot.background=element_rect(fill="white",color="white"))
