library(tidyverse)
library(vegan)
library(ggvegan)
library(ggalt)
library(ggrepel)


betadiv<-betadiv %>% 
  as.data.frame()
betadiv

betadivnmds<-metaMDS(betadiv[16:50],distance="jaccard",k=2, autotransform=F)
betadivnmds

ggbetadivnmds<-fortify(betadivnmds)
ggbetadivnmds<-ggbetadivnmds[1:22,]
ggbetadivnmds$bioma<-betadiv$Bioma
ggbetadivnmds

ggbetadivnmds$bioma[ggbetadivnmds$bioma=="Mata-Atlântica (z. m)"]<-"Mata-Atlântica"
ggbetadivnmds$bioma[ggbetadivnmds$bioma=="Mata-Atlântica (agreste)"]<-"Mata-Atlântica"

gghullbetadiv<-ggbetadivnmds %>% 
  group_by(bioma) %>% 
  slice(chull(NMDS1,NMDS2))

ggbetadivnmds %>% 
  ggplot(aes(NMDS1,NMDS2,fill=bioma))+
  geom_label(aes(label=Label),color="black",size=3)+
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
      


