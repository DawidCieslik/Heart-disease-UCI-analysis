library(tidyverse)
library(GGally)
library(ggplot2)

generateScatterPlot<-function(data_in){
  
    #Simpler Scatterplot Matrix with some of the attributes
    png(filename = "images/scatterplotMatrix1.png")
    pairs(~age+trestbps+chol+thalach, data=data_in,
        labels=c("Wiek", "Spoczynkowe cisnienie krwi","Cholesterol","Maksymalne tetno"),
        main="Wykres typu Scatterplot Matrix dla wybranych atrybutow", panel=panel.smooth)
    dev.off()

    #More advanced Scatterplot Matrix- with correlation and density functions
    plot2 <- ggpairs(data=data_in,columns=c("age","trestbps","chol","thalach"),
        columnLabels=c("Wiek", "Spoczynkowe cisnienie krwi","Cholesterol","Maksymalne tetno"),
        title="Wykres typu Scatterplot Matrix dla wybranych atrybutow",lower=list(continuous="smooth"))
    ggsave("images/ScatterplotMatrix2.png", plot2)
}