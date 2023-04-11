### Plotly 3D surface

library(plotly)

data("volcano")
volcano

fig <- plot_ly(z = ~RED)
fig <- fig %>% add_surface()

fig


### Axis labeling
axx <- list(
  title = "Homophily"
)

axy <- list(
  title = "Average Threshold"
)

axz <- list(
  title = "Revolt Rate"
)

y = sapply(thres, mean)

x = p
y = y
z = RED


fig <- plot_ly(x = ~x, y = ~y, z = ~z, type = "surface") 

fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig



fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 0, end = 1, size = 0.01, color = 'red'),
    z = list(show = TRUE, start = 0, end = 1, size = 0.05)),
  x = ~x,
  y = ~y,
  z = ~z)






