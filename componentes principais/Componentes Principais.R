# AN??LISE DE COMPONENTES PRINCIPAIS (PCA) E AN??LISE FATORIAL

# Limpar ambiente
rm(list = ls())

# Definir diret??rio de trabalho
setwd("C:/Users/PC GAMER/Downloads/data")

# Carregar pacotes (descomente se precisar instalar)
# install.packages("REdaS")
# install.packages("psych")
# install.packages("ggplot2")
library(REdaS)
library(psych)
library(ggplot2)

# Carregar a base de dados
dados <- read.csv("pca.csv")

# Selecionar apenas as colunas num??ricas
dados_num <- dados[sapply(dados, is.numeric)]

# Verificar estrutura e estat??sticas b??sicas
cat("Estrutura dos dados num??ricos:\n")
str(dados_num)
cat("\nResumo estat??stico:\n")
print(summary(dados_num))

# Matriz de correla????o
cat("\nMatriz de correla????o:\n")
print(round(cor(dados_num, use = "complete.obs"), 2))

# ------------------------------------------------------------------------------
# 1. An??lise de Componentes Principais (PCA)
# ------------------------------------------------------------------------------
cat("\nAn??lise de Componentes Principais (PCA):\n")
pca_modelo <- princomp(dados_num, scores = TRUE, cor = TRUE)

# Vari??ncia explicada por componente
print(summary(pca_modelo))

# Cargas fatoriais (loadings)
cat("\nCargas dos Componentes:\n")
print(loadings(pca_modelo))

# Scree plot (gr??fico de autovalores)
screeplot(pca_modelo, type = "barplot", main = "Gr??fico Scree - Autovalores",
          col = "lightblue", border = "blue")


# Biplot
biplot(pca_modelo, choices = 1:2, main = "Biplot dos Componentes Principais",
       xlabs = rep(".", nrow(dados_num)))  # n??o mostra os labels originais
points(pca_modelo$scores[1:50, 1], pca_modelo$scores[1:50, 2], pch = 16, col = "blue")


# Scores das primeiras 10 observa????es
cat("\nScores dos 10 primeiros casos:\n")
print(round(pca_modelo$scores[1:10, ], 3))

# ------------------------------------------------------------------------------
# 2. Rota????es para melhor interpreta????o
# ------------------------------------------------------------------------------
cat("\nRota????es dos Componentes:\n")
print("Rota????o Varimax:")
print(varimax(pca_modelo$loadings[, 1:3]))

print("Rota????o Promax:")
print(promax(pca_modelo$loadings[, 1:3]))

# ------------------------------------------------------------------------------
# 3. An??lise Fatorial (com 3 fatores)
# ------------------------------------------------------------------------------
cat("\nAn??lise Fatorial com 3 fatores (rota????o varimax):\n")
fa_modelo <- factanal(dados_num, factors = 3, rotation = "varimax", scores = "regression")
print(fa_modelo)

# Scores fatoriais das 10 primeiras observa????es
cat("\nScores fatoriais (10 primeiros casos):\n")
print(head(fa_modelo$scores, 10))

# ------------------------------------------------------------------------------
# 4. Testes de Adequa????o: KMO e Bartlett
# ------------------------------------------------------------------------------
cat("\nEstat??stica KMO:\n")
print(KMOS(dados_num))

cat("\nTeste de Esfericidade de Bartlett:\n")
print(bart_spher(dados_num))

# ------------------------------------------------------------------------------
# 5. Conclus??o
# ------------------------------------------------------------------------------
cat("\nConclus??o:\n")
cat("- Verifique a vari??ncia explicada pelos componentes principais.\n")
cat("- Analise as cargas fatoriais rotacionadas para interpretar os fatores.\n")
cat("- KMO > 0.6 e Bartlett com p < 0.05 indicam adequa????o da an??lise fatorial.\n")
