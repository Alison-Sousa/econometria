# Econometria Espacial em R

# Instalar e carregar pacotes necessários
library(spdep)
library(spatialreg)
library(ggplot2)
library(sf)

# Definir diretório de trabalho
setwd("C:/Users/PC GAMER/Downloads/data")

# Carregar dados
df <- read.csv("data.csv")

# Variáveis
Y <- df$CRIME
X <- df[, c("INC", "HOVAL")]
coords <- cbind(df$X, df$Y)

# Converter para objeto sf para visualização
df_sf <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)

# Criar vizinhança por distância
neighbors <- dnearneigh(coords, d1=0, d2=10)
listw <- nb2listw(neighbors, style="W")

# 🔍 VISUALIZAÇÃO BONITA: mapa de pontos com linhas de vizinhança
# Transformar para objeto spatial (necessário para os gráficos de linhas)
df_sp <- as(df_sf, "Spatial")

# Plotar com ggplot2
plot_neighbors <- function(coords, nb, title = "Mapa de Vizinhança Espacial") {
  segments <- list()
  for (i in seq_along(nb)) {
    for (j in nb[[i]]) {
      segments[[length(segments) + 1]] <- data.frame(
        x = coords[i, 1], y = coords[i, 2],
        xend = coords[j, 1], yend = coords[j, 2]
      )
    }
  }
  seg_df <- do.call(rbind, segments)
  coords_df <- as.data.frame(coords)
  
  ggplot() +
    geom_segment(data = seg_df, aes(x = x, y = y, xend = xend, yend = yend), color = "gray60", alpha = 0.6) +
    geom_point(data = coords_df, aes(x = V1, y = V2), color = "darkred", size = 3) +
    theme_minimal() +
    labs(title = title, x = "Coordenada X", y = "Coordenada Y")
}

# Mostrar gráfico
plot_neighbors(coords, neighbors)

# Estatísticas descritivas
summary(Y)
summary(X)

# OLS
olsreg <- lm(Y ~ INC + HOVAL, data = df)
summary(olsreg)

# Moran's I
moran.test(Y, listw)

# Gráfico de Moran (ajustado)
moran.plot(Y, listw, labels = FALSE, col = "steelblue", pch = 20, xlab = "Variável original", ylab = "Variável espacialmente defasada")

# Testes LM
lm.RStests(olsreg, listw, test = c("LMlag", "LMerr"))

# Modelos espaciais com contiguidade
spatial.lag <- spatialreg::lagsarlm(Y ~ INC + HOVAL, data = df, listw = listw)
summary(spatial.lag)

spatial.error <- spatialreg::errorsarlm(Y ~ INC + HOVAL, data = df, listw = listw)
summary(spatial.error)

# Vizinho baseado em distância (d = 10)
nb_dist <- dnearneigh(coords, d1=0, d2=10)
listw_dist <- nb2listw(nb_dist, style="W")

# Moran com pesos por distância
moran.test(Y, listw_dist)
moran.plot(Y, listw_dist, labels = FALSE, col = "tomato", pch = 16)

# Testes LM
lm.LMtests(olsreg, listw_dist, test = c("LMlag", "LMerr"))

# Modelos espaciais com matriz por distância
spatial.lag_dist <- spatialreg::lagsarlm(Y ~ INC + HOVAL, data = df, listw = listw_dist)
summary(spatial.lag_dist)

spatial.error_dist <- spatialreg::errorsarlm(Y ~ INC + HOVAL, data = df, listw = listw_dist)
summary(spatial.error_dist)
