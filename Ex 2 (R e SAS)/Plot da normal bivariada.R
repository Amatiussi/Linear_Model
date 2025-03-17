install.packages("plotly")    # Este pacote permite confeccionar gráficos dinâmicos tridimensionais
library(plotly)

y1 <- seq(from = -4, to = 4, by = 0.1); y1
y2 <- seq(from = -4, to = 4, by = 0.1); y2

r <- -0.75  # fixa o coeficiente de correlação entre y1 e y2
pi   # valor de pi (3.141593)

# cálculo da densidade bivariada
z <- outer(y1, y2, function(y1,y2) { 
  phi <- 1/(2 * pi * sqrt(1 - r^2)) * exp(-(y1^2 - 2 * r * y1 * y2 + y2^2) / (2 * (1 - r^2)))
  return(phi)
})

# criar o gráfico
plot_ly(x = y1, y = y2, z = z, type = "surface") |> 
  layout(title = paste("Densidade Normal Bivariada (r =", r, ")"))
