library(tidyverse) # includes many useful data analysis tools
library(ggplot2)


generateBoxAndWhiskers <- function(data){
  # Age
  ggplot(dataClean,aes(y=age)) +
    geom_boxplot(fill="coral1")+
    ggtitle("Wykres pudełkowy wieku") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y="Wiek")
  
  ggsave("images/boxplot-age.png")
  
  # Resting blood Pressure
  ggplot(data,aes(y=trestbps)) +
    geom_boxplot(fill="coral1")+
    ggtitle("Wykres pudełkowy ciśnienia spoczynkowego") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y="Ciśnienie spoczynkowe")
  
  ggsave("images/boxplot-trestbps.png")
  
  # Cholesterole
  ggplot(data,aes(y=chol)) +
    geom_boxplot(fill="coral1")+
    ggtitle("Wykres pudełkowy poziomu cholesterolu") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y="Poziom cholesterolu")
  
  ggsave("images/boxplot-chol.png")
  
  # thalach - maximum heart rate achieved
  ggplot(data,aes(y=thalach)) +
    geom_boxplot(fill="coral1")+
    ggtitle("Wykres pudełkowy maksymalnego zarejestrowanego tętna") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y="Maksymalne zarejestrowane tętno")
  
  ggsave("images/boxplot-thalach.png")
  
  
  # oldpeak - ST depression induced by exercise relative to rest
  ggplot(data,aes(y=oldpeak)) +
    geom_boxplot(fill="coral1")+
    ggtitle("Względne obniżenie rejonu ST w w czasie aktywności fizycznej") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y="Względne obniżenie rejonu ST")
  
  ggsave("images/boxplot-oldpeak.png")
}