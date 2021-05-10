library(ggplot2)

thalachTarget <- function(data){
  x <- as.numeric(data$thalach)
  y <- as.numeric(data$target)
  
  cor.test(x, y, method="kendall", alternative="less")
  
  ggplot(data, aes(target, thalach)) +
    geom_boxplot() +
    ggtitle("Wykres pudełkowy maksymalnego tętna w grupach diagnozy") + 
    labs(x="Stopień prawdopodobieństwa choroby serca", y="Maksymalne tętno podczas próby wysiłkowej [bpm]")
  
  ggsave("images/thalach-target.png")
  
}
