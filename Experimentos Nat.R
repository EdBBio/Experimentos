Bioac<-Bioac %>% 
  mutate("População"=case_when(Amostra==1:4~"1",
                               Amostra==5:8~"2")) %>% 
  as.data.frame()

Bioac<-Bioac %>% 
  pivot_longer(names_to = "Tipo de Frequência",
               values_to = "Frequência",
               cols = starts_with("Freq"))
Bioac<-Bioac %>% 
  as.data.frame()

Bioac$`Duração_nota (s)`<-as.numeric(Bioac$`Duração_nota (s)`)

class(Bioac$`Duração_nota (s)`)

Bioac %>% 
  ggplot(aes(População, Frequência,fill=População))+
  geom_boxplot()+
  labs(y="Frequência [Hz]")+
  facet_wrap(~`Tipo de Frequência`)+
  tema

Bioac %>% 
  ggplot(aes(População, `Duração_nota (s)`,fill=População))+
  geom_boxplot()+
  labs(y="Frequência [Hz]")+
  facet_wrap(~`Tipo de Frequência`)+
  theme(text=element_text(family="TimesNewRoman"),
        title=element_text(color="black",size=15),
        axis.text = element_text(color="black",size=10),
        axis.title = element_text(color="black",size=10),
        panel.grid=element_line(color="grey75",linetype="dashed",size=0.7),
        axis.line=element_blank(),
        plot.background=element_rect(fill="white",color="white"),
        panel.background=element_rect(fill="white"),
        panel.border = element_rect(colour = "black",fill=NA,size=0.5),
        legend.key= element_rect(color="white",fill="white"))

