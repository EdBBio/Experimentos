GGTukey<-function(Tukey){
  A<-require("tidyverse")
  if(A==TRUE){
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
  B<-as.data.frame(Tukey[1])
  colnames(B)[2:4]<-c("lwr","upr","p")
  C<-data.frame(id=row.names(B),
                min=B$lwr,
                max=B$upr,
                p=B$p)
  D<-C%>%
    ggplot(aes(id))+
    geom_errorbar(aes(ymin=min,
                      ymax=max),
                  width = 0.2)+
    geom_hline(yintercept=0,
               color="red")+
    coord_flip()+
    theme(title=element_text(color="black"),
          axis.text=element_text(color="black"),
          axis.line=element_line(color="black"),
          panel.grid=element_line(color="gray"),
          panel.background=element_rect(fill="white"),
          plot.background=element_rect(fill="white",color="white"))
  return(D)
}

theme<-theme(title=element_text(color="black"),
             axis.text=element_text(color="black"),
             axis.line=element_line(color="black"),
             panel.grid=element_line(color="gray"),
             panel.background=element_rect(fill="white"),
             plot.background=element_rect(fill="white",color="white"))