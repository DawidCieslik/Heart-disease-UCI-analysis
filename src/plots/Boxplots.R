library(ggplot2)


generateBoxPlots <- function(data){

  
  # Diagnosis vs age
  ggplot(data, aes(x=target, y=age)) + 
    geom_boxplot(fill="coral") +
    ggtitle("Klasa diagnozy a wiek") +
    labs(x="Klasa diagnozy", y="Wiek")
  
  ggsave("images/boxplot-target-vs-age.png")
  
  # Diagnosis vs resting blood pressure
  ggplot(data, aes(x=target, y=trestbps)) + 
    geom_boxplot(fill="cadetblue1") +
    ggtitle("Klasa diagnozy a cisnienie spoczynkowe") +
    labs(x="Klasa diagnozy", y="Cisnienie spoczynkowe")
  
  ggsave("images/boxplot-target-vs-trestbps.png")
  
  # Diagnosis vs cholesterol
  ggplot(data, aes(x=target, y=chol)) + 
    geom_boxplot(fill="brown2") +
    ggtitle("Klasa diagnozy a poziom cholesterolu") +
    labs(x="Klasa diagnozy", y="Cholesterol [mg/dl]")
  
  ggsave("images/boxplot-target-vs-chol.png")
  
  # Diagnosis vs max BPS
  ggplot(data, aes(x=target, y=thalach)) + 
    geom_boxplot(fill="aquamarine3") +
    ggtitle("Klasa diagnozy a maksymalne tetno") +
    labs(x="Klasa diagnozy", y="Maksymalna wartosc tetna")
  
  ggsave("images/boxplot-target-vs-thalach.png")
  
  # Diagnosis vs oldpeak
  ggplot(data, aes(x=target, y=oldpeak)) + 
    geom_boxplot(fill="deeppink3") +
    ggtitle("Klasa diagnozy a obnizenie odcinka ST") +
    labs(x="Klasa diagnozy", y="Obnizenie odcinka ST [mV]")
  
  ggsave("images/boxplot-target-vs-oldpeak.png")
  
  # Diagnosis vs CA results
  ggplot(data, aes(x=target, y=ca)) + 
    geom_boxplot(fill="darkolivegreen3") +
    ggtitle("Klasa diagnozy a wynik koronografii") +
    labs(x="Klasa diagnozy", y="Liczba glownych naczyn")
  
  ggsave("images/boxplot-target-vs-ca.png")
  return()
}