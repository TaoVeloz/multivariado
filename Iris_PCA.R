library(tidyverse)
# cargamos datos
iris
library(GGally)

ggpairs(iris, mapping = aes(col = Species)) +
  theme_bw()

# tomamos la parte numerica
data_iris <- iris[1:4]

#calculamos componentes principales
PCA_data <- prcomp(data_iris)

#PCA_data$sdev^2

map_dfc(1:4, ~PCA_data$rotation[, .] * sqrt(PCA_data$sdev ^ 2)[.])


### graficar 

#install.packages("factoextra")
library(factoextra)
PCA <- get_pca(PCA_data)
fviz_pca_biplot(PCA_data, label = "var")
fviz_pca_var(PCA_data)
fviz_screeplot(PCA_data, addlabels = TRUE, choice = "eigenvalue")
fviz_screeplot(PCA_data, addlabels = TRUE, choice = "variance")

# UBICACION DE LA Especies
PCA_plot <- iris %>%
  mutate(PCA1 = PCA_data$x[, 1], PCA2 = PCA_data$x[, 2],)
ggplot(PCA_plot, aes(PCA1, PCA2, col = Species)) +
  geom_point() +
  theme_bw()

# Agregue una capa stat_ellipse () al gráfico de la figura 
# para agregar elipses de confianza del 95% a cada clase de billete.

ggplot(PCA_plot, aes(PCA1, PCA2, col = Species)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()


