# Outlier Detect

outlier_detect <- function(data){
  
  A <- require("tidyverse")
  
  if(A == TRUE){
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  summarised_data <- summary(data)
  
  interquartil_space <- (summarised_data[5] - summarised_data[2]) * 1.5
  
  upper_outlier <- summarised_data[5] + interquartil_space
  
  lower_outlier <- summarised_data[2] - interquartil_space
  
  upper_outlier_condition <- if(any(data %>% na.omit() > upper_outlier) == TRUE){
    message("upper outlier: TRUE")
    print(data %>%
            as.data.frame %>%
            dplyr::filter(data > summarised_data[5]) %>% 
            dplyr::filter(. > upper_outlier) %>%
            dplyr::pull() %>% 
            as.numeric())
  } else {
    message("upper outlier: FALSE")
  } 
  
  lower_outlier_condition <- if(any(data %>% na.omit() < lower_outlier) == TRUE) {
    message("lower outlier: TRUE")
    print(data %>%
            as.data.frame %>%
            dplyr::filter(data < summarised_data[2]) %>% 
            dplyr::filter(. < lower_outlier) %>%
            dplyr::pull() %>% 
            as.numeric())
  } else {
    message("lower outlier: FALSE")
  }
}

# gg_normaloty

gg_normality <- function(data) {
  
  if(require("tidyverse") == FALSE){ # Caso quem usar a função não estiver com o Tidyverse instalado, a função instalará e o ativará
    install.packages("tidyverse")
    library(tidyverse)
  } else { # Caso quem usar a função já estiver com o Tidyverse instalado, a função apenas o carregará
    library(tidyverse)
  }
  
  shapiro <- shapiro.test(data) # Fazendo o teste de Shapiro-Wilki para o vetor selecionado na função
  
  w <- shapiro[[1]] # Extraindo o valor da estaística W do teste
  
  w <- str_remove(w, "W") # Removendo o String "W", apenas ficando o número  da estatística
  
  p <- shapiro[[2]] # Extraindo o valor de P do teste
  
  data %>% # Carregando o dataset base do ggplot
    as.data.frame %>% # Transformando em dataframe
    ggplot(aes(sample = data))+ # Usando o comando sample = nos dados selcionados na função
    stat_qq(shape = 21, # Pontos dos quantis
            color = "darkred",
            fill = "orange",
            size = 4.5)+
    stat_qq_line(color = "darkblue", # Linha dos Quantis
                 size = 0.75)+
    labs(subtitle = str_glue("W = ", {w}, ", p = ", {p}))+ # Usando a função str_glue() no subtítulo para adicionar a estatística W e o valor de P
    theme(title = element_text(color = "black", 
                               size = 10), # Tema do Gráfico
          axis.text = element_text(color = "black", 
                                   size = 10),
          plot.subtitle = element_text(color = "black", 
                                       size = 10),
          axis.title = element_text(color = "black", 
                                    size = 10),
          panel.grid = element_line(color = "grey75", 
                                    linetype = "dashed", 
                                    size = 0.7),
          panel.grid.minor = element_line(color = "grey75", 
                                          linetype = "dashed", 
                                          size = 0.7),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", 
                                         color = "white"),
          panel.background = element_rect(fill ="white"),
          panel.border = element_rect(colour = "black", 
                                      fill = NA, 
                                      size = 0.5),
          legend.key = element_rect(color = "white", 
                                    fill = "white"))
}