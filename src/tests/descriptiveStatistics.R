library(moments)

DescriptiveStatistics <- function(data)
{
  #Obliczanie mediany
  median(data$age)
  median(data$thalach)
  median(data$trestbps)
  median(data$chol)
  
  #Obliczanie średniej arytmetycznej
  mean(data$age)
  mean(data$thalach)
  mean(data$trestbps)
  mean(data$chol)

  #Obliczanie współczynnika skośności
  skewness(data$age)
  skewness(data$thalach)
  skewness(data$trestbps)
  skewness(data$chol)
  
  #Obliczanie kurtozy
  kurtosis(data$age)
  kurtosis(data$thalach)
  kurtosis(data$trestbps)
  kurtosis(data$chol)
}

