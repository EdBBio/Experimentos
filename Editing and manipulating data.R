# Utilizando o package DataEditR

library(DataEditR)

data("iris")

data_edit(iris)

plot(iris)

# utilizando o dplyr::case_when()

library(tidyverse)

Niris<-iris %>% 
  mutate(profile=case_when(
    Sepal.Length >= 5 ~ "Yes",
    Sepal.Length < 5 ~ "Not"
    ))
Niris

# Utilizando o forcats::fct_recorde()

Niris$profile<-fct_recode(Niris$profile,
           Sim="Yes",
           Não='Not')

Niris<-iris %>% 
  mutate(profile=case_when(iris$Sepal.Length >= 5 ~ "Yes",
                           iris$Sepal.Length < 5 ~ "Not"))

Niris$perfil=fct_recode(Niris$profile,
                  Sim="Yes",
                  Não='Not')

# Utilizando outros comandos

Niris$profile

Niris$profile[Niris$profile=="Yes"]<-"sim"
Niris$profile[Niris$profile=="Not"]<-"não"
Niris

zonas <- paste0("zona ",1:20)
locais <- paste0("local ",1:20)
dadosMon <- data.frame(area = zonas,locais=locais)
dadosMon
dadosMon[dadosMon==c("zona 1","zona 2","zona 3")]<-"nova zona"
dadosMon

class(dadosMon$area)
edit(iris)




library(tidyverse)
mtcars

t1<-mtcars  %>% 
  filter(if_all(.cols = everything(), ~ . != 4)) %>% 
  as.data.frame()
t2<-mtcars  %>% 
  filter(if_any(.cols = everything(), ~ . == 4)) %>% 
  as.data.frame()
t1==t2


starwars %>% 
  filter()

t3<-starwars %>% 
  filter(if_any(.cols = everything(),~.=="Human")) %>% 
  as.data.frame()
  
t4<-starwars %>% 
  filter(!if_any(.cols = everything(),~.=="Human") & !if_any(.cols = everything(),~.=="preto")) %>% 
  as.data.frame()

nrow(t3)==nrow(t4)
ncol(t3)==ncol(t4)

iris %>% 
  filter(.cols=)


?.col()

iris %>% 
  filter(!if_any(.cols=everything(),~.<5))
t<-mtcars %>% 
  filter(!if_any(.cols=everything(),~.==5)) 
nrow(mtcars)==nrow(t)

starwars %>% 
  replace(starwars=="blue",values="azul")

data("starwars")
repla

data("starwars")

data("iris")

iris %>% 
  replace(iris=="setosa",values="Irissetosa")

starwars



update.packages(checkBuilt=TRUE, ask=FALSE)
