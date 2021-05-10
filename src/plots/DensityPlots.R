library(ggplot2)

generateDensityPlots <- function(data)
{
  ggplot(data,aes(x=age)) +
    geom_density(fill="steelblue")+
    ggtitle("Wykres gêstoœci wieku") + 
    labs(x="Wiek", y="Gêstoœæ")
  ggsave("images/density-age.png")
  
  ggplot(data,aes(x=trestbps)) +
    geom_density(fill="steelblue")+
    ggtitle("Wykres gêstoœci ciœnienia spoczynkowego") + 
    labs(x="Ciœnienie spoczynkowe", y="Gêstoœæ")
  ggsave("images/density-trestbps.png")
  
  ggplot(data,aes(x=chol)) +
    geom_density(fill="steelblue")+
    ggtitle("Wykres gêstoœci poziomu cholesterolu") + 
    labs(x="Poziom cholesterolu", y="Gêstoœæ")
  ggsave("images/density-chol.png")
  
  ggplot(data,aes(x=thalach)) +
    geom_density(fill="steelblue")+
    ggtitle("Wykres gêstoœci maksymalnego zarejestrowanego têtna") + 
    labs(x="Maksymalne zarejestrowane têtno", y="Gêstoœæ")
  ggsave("images/density-thalach.png")
  
  ggplot(data,aes(x=oldpeak)) +
    geom_density(fill="steelblue")+
    ggtitle("Wykres gêstoœci wzglêdnego obni¿enia rejonu ST w w czasie aktywnoœci fizycznej") + 
    labs(x="Wzglêdne obni¿enie rejonu ST w w czasie aktywnoœci fizycznej", y="Gêstoœæ")
  ggsave("images/density-oldpeak.png")
}