library(tidyverse)
library(ggplot2)

generate2DDensity <- function(data){

  #Age vs blood pressure
    ggplot(data,aes(age,trestbps)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości wieku względem ciśnienia krwi") + 
        labs(y="Ciśnienie")+
        labs(x="Wiek")+
        theme(legend.position = "none")

    ggsave("images/2d-density-age-bps.png")

  #Age vs heart rate
    ggplot(data,aes(age,thalach)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości wieku względem tętna spoczynkowego") + 
        labs(y="Tętno")+
        labs(x="Wiek")+
        theme(legend.position = "none")

    ggsave("images/2d-density-age-hr.png")
    
  #Age vs cholesterol
    ggplot(data,aes(age,chol)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości wieku względem poziomu cholesterolu") + 
        labs(y="Cholesterol")+
        labs(x="Wiek")+
        theme(legend.position = "none")

    ggsave("images/2d-density-age-chol.png")

  #Heart rate vs blood pressure
    ggplot(data,aes(thalach,trestbps)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości tętna względem ciśnienia krwi") + 
        labs(y="Ciśnienie")+
        labs(x="Tętno")+
        theme(legend.position = "none")

    ggsave("images/2d-density-hr-bps.png")

  #Heart rate vs cholesterol
    ggplot(data,aes(thalach,chol)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości tętna względem ciśnienia krwi") + 
        labs(y="Cholesterol")+
        labs(x="Tętno")+
        theme(legend.position = "none")

    ggsave("images/2d-density-hr-chol.png")

  #Cholesterol vs blood pressure
    ggplot(data,aes(chol,trestbps)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości tętna względem ciśnienia krwi") + 
        labs(y="Ciśnienie")+
        labs(x="Tętno")+
        theme(legend.position = "none")

    ggsave("images/2d-density-chol-bps.png")

  #Oldpeak vs blood pressure
    ggplot(data,aes(oldpeak,trestbps)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości względnego obniżenia rejonu ST względem ciśnienia krwi") + 
        labs(y="Ciśnienie")+
        labs(x="Względne obniżenie rejonu ST")+
        theme(legend.position = "none")

    ggsave("images/2d-density-oldpeak-bps.png")
    
  #Oldpeak vs heart rate
    ggplot(data,aes(oldpeak,thalach)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości względnego obniżenia rejonu ST względem tętna spoczynkowego") + 
        labs(y="Tętno")+
        labs(x="Względne obniżenie rejonu ST")+
        theme(legend.position = "none")

    ggsave("images/2d-density-oldpeak-hr.png")
    
  #Oldpeak vs cholesterol
    ggplot(data,aes(oldpeak,chol)) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")+
        ggtitle("Wykres gęstości względnego obniżenia rejonu ST względem cholesterolu") + 
        labs(y="Cholesterol")+
        labs(x="Względne obniżenie rejonu ST")+
        theme(legend.position = "none")

    ggsave("images/2d-density-oldpeak-chol.png")
}