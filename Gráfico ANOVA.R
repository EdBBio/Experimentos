ANOVA<-function(Base){
 A<-require("tidyverse")
  if(A==TRUE){
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
 source("G:/Meu Drive/R/Experimentos/funções.R")
 colnames(Base)[1:2]<-c("valores","grupos")
 B<-aov(Base$valores~Base$grupos)
 C<-summary(B)
 D<-C[[1]][["Pr(>F)"]]
 E<-Base %>% 
    ggplot(aes(grupos,valores,
           fill=grupos))+
    geom_boxplot(color="black")+
    labs(x=NULL,
         y=NULL,
         fill="Grupos")+
    scale_fill_manual(values=c("red","yellow","green","blue"))+
    theme
 F<-if(D<0.05){
   G<-TukeyHSD(B)
   H<-GGTukey(G)
 }
 return(list(C,E,F))
}


ANOVA(testeanv)

colnames(testeanv)[1:2]<-c("valores","grupos")
B<-aov(testeanv$valores~testeanv$grupos)
B
source("G:/Meu Drive/R/Experimentos/funções.R")

geom_tukey<-function(Tukey, Style){
  GT<-function(Tukey){
    A<-require("tidyverse")
    if(A==TRUE){
      library(tidyverse)
    } else {
      install.packages("tidyverse")
      library(tidyverse)
    }
    B<-as.data.frame(Tukey[1])
    colnames(B)[2:3]<-c("min",
                        "max")
    C<-data.frame(id=row.names(B),
                  min=B$min,
                  max=B$max)
    D<-C%>%
      ggplot(aes(id))+
      geom_errorbar(aes(ymin=min,
                        ymax=max),
                    width = 0.2)+
      geom_hline(yintercept=0,
                 color="red")+
      labs(x=NULL)+
      coord_flip()+
      theme(title=element_text(color="black"),
            axis.text=element_text(color="black"),
            axis.line=element_line(color="black"),
            panel.grid=element_line(color="gray"),
            panel.background=element_rect(fill="white"),
            plot.background=element_rect(fill="white",color="white")
      )
    return(D)
  }
  GT2<-function(Tukey){
    A<-require("tidyverse")
    if(A==TRUE){
      library(tidyverse)
    } else {
      install.packages("tidyverse")
      library(tidyverse)
    }
    B<-as.data.frame(Tukey[1])
    colnames(B)[2:4]<-c("min",
                        "max",
                        "p")
    C<-data.frame(id=row.names(B),
                  min=B$min,
                  max=B$max,
                  idt=ifelse(B$p<0.05,
                             "significant",
                             "not significant")
    )
    D<-C%>%
      ggplot(aes(id,color=idt))+
      geom_errorbar(aes(ymin=min,
                        ymax=max),
                    width = 0.5,
                    size=1.25)+
      labs(x=NULL,
           color=NULL)+
      scale_color_manual(values=c("red",
                                  "green")
      )+
      coord_flip()+
      theme(title=element_text(color="black"),
            axis.text=element_text(color="black"),
            axis.line=element_line(color="black"),
            panel.grid=element_line(color="gray"),
            panel.background=element_rect(fill="white"),
            legend.key=element_rect(fill="white"),
            plot.background=element_rect(fill="white",color="white")
      )
    return(D)
  }
  GR<-if(Style==1){
    return(GT(Tukey))
  } else if(Style==2){
    return(GT2(Tukey))
  }
  return(GR)
}

geom_tukey(tukeyteste,Style=1)
