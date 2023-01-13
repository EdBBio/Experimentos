# pacotes

library(tidyverse) # pacote para fazer manipulação de dados e os ggplots
library(vegan) # pacote para calcular o nMDS
library(ggvegan) # pacote para tranformar os dados do nMDS em um dataset

# Importando os dados

sítio<-read.csv2("dataset_sitio7.csv",sep=";")

sítio

# Calculando o nMDS

sitionmds<-metaMDS(sítio,distance = "bray",k=2,autotransform = F) # fazendo o nMDS e atribuindo ao objeto chamado sitionmds
sitionmds # Conferindo os dados

# Extraindo os dados do nMDS para um dataset

ggsitio<-fortify(sitionmds) # extraindo os dados
ggsitio<-ggsitio[1:25,] # seelecionando apenas os scores dos locais
ggsitio<-ggsitio %>% # criando uma nova variável condicional baseado na numeração de cada ponto para criar os tipos de sítio
  mutate("sitio"=case_when(Label==1:5~"Serra Branca (trilha)",
                           Label==6:10~"Chapadão (trilha)",
                           Label==11:15~"Estrada da toca do vale (estrada)",
                           Label==16:20~"Trilha do Sítio Arqueológico Alcobaça (trilha)",
                           Label==21:25~"Estrada da Zona Rural próxima a alcobaça (estrada)"),
         "categoria"=case_when(Label %in% c(1:10,16:20)~"trilha",
                               Label %in% c(11:15,21:25)~"estrada"))


ggsitio # conferindo os dados

# Criando os datasets dos polígonos

ggpol<-ggsitio %>% 
  group_by(sitio) %>% 
  slice(chull(NMDS1,NMDS2)) # baseado nos sítios

ggpol2<-ggsitio %>% 
  group_by(categoria) %>% 
  slice(chull(NMDS1,NMDS2)) # baseado nas categorias

# Criando os ggplots

ggsitio %>% 
  ggplot(aes(NMDS1,NMDS2,color=sitio,fill=sitio))+
  geom_polygon(data=ggpol,alpha=0.3,size=0.7)+
  geom_label(aes(label=Label),size=4,color="black",show.legend = F)+
  scale_fill_manual(values = c("#2D91AD","#FF7A00","#968807","#10FF13","#A2388A"))+
  scale_color_manual(values = c("#2D91AD","#FF7A00","#968807","#10FF13","#A2388A"))+
  theme(text=element_text(family="TimesNewRoman"),
        title=element_text(color="black",size=15),
        axis.text = element_text(color="black",size=10),
        axis.title = element_text(color="black",size=10),
        panel.grid=element_line(color="grey75",linetype = "dashed",size=.7),
        axis.line=element_blank(),
        plot.background=element_rect(fill="white",color="white"),
        panel.background=element_rect(fill="white"),
        panel.border = element_rect(colour = "black", fill = NA,size=0.5),
        legend.key= element_rect(color="white",fill="white")) # ggplot dos 5 sítios
  
  
ggsitio %>% 
  ggplot(aes(NMDS1,NMDS2,color=categoria,fill=categoria))+
  geom_polygon(data=ggpol2,alpha=0.3,size=0.7)+
  geom_label(aes(label=Label),size=4,color="black",show.legend = F)+
  theme(text=element_text(family="TimesNewRoman"),
        title=element_text(color="black",size=15),
        axis.text = element_text(color="black",size=10),
        axis.title = element_text(color="black",size=10),
        panel.grid=element_line(color="grey75",linetype = "dashed",size=.7),
        axis.line=element_blank(),
        plot.background=element_rect(fill="white",color="white"),
        panel.background=element_rect(fill="white"),
        panel.border = element_rect(colour = "black", fill = NA,size=0.5),
        legend.key= element_rect(color="white",fill="white")) # ggplot das 2 categoria
  
  
  