# ===================================================================
# Configuração Inicial e Carregamento de Dados
# ===================================================================

# Definir o diretório onde está o arquivo CSV
setwd("C:/Users/PC GAMER/Downloads/data")

# Carregar o dataset
auto <- read.csv("auto.csv")

# Visualizar estrutura da base
str(auto)

# ===================================================================
# Análise Descritiva das Variáveis de Interesse
# ===================================================================

# Estatísticas descritivas
summary(auto[, c("mpg", "weight1", "price", "foreign")])

# Matriz de correlação
cor(auto[, c("mpg", "weight1", "price", "foreign")])

# ===================================================================
# Gráfico de Dispersão: mpg vs weight1 (com linha de tendência)
# ===================================================================

library(ggplot2)

ggplot(auto, aes(x = weight1, y = mpg)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relação entre Peso do Veículo (weight1) e Consumo (mpg)",
       x = "Peso do veículo (weight1)",
       y = "Milhas por galão (mpg)") +
  theme_minimal(base_size = 14)

# ===================================================================
# Modelo de Regressão Linear Simples
# ===================================================================

modelo_simples <- lm(mpg ~ weight1, data = auto)
summary(modelo_simples)

# Intervalo de confiança dos coeficientes
confint(modelo_simples)

# Diagnóstico gráfico do modelo simples
par(mfrow = c(2, 2))
plot(modelo_simples)
par(mfrow = c(1, 1))

# ===================================================================
# Resíduos e Valores Ajustados - Modelo Simples
# ===================================================================

auto$yhat1 <- fitted(modelo_simples)
auto$res1 <- resid(modelo_simples)

# Resíduos vs Ajustados
ggplot(auto, aes(x = yhat1, y = res1)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos vs Valores Ajustados (Modelo Simples)",
       x = "Valores Ajustados",
       y = "Resíduos") +
  theme_light()

# ===================================================================
# Regressão Linear Múltipla
# ===================================================================

modelo_multiplo <- lm(mpg ~ weight1 + price + foreign, data = auto)
summary(modelo_multiplo)
confint(modelo_multiplo)

# ===================================================================
# Resíduos e Valores Ajustados - Modelo Múltiplo
# ===================================================================

auto$yhat2 <- fitted(modelo_multiplo)
auto$res2 <- resid(modelo_multiplo)

# QQ-Plot dos resíduos (verificação de normalidade)
library(car)
qqPlot(modelo_multiplo, main = "QQ-Plot dos Resíduos (Modelo Múltiplo)")

# Resíduos vs Ajustados - Modelo Múltiplo
ggplot(auto, aes(x = yhat2, y = res2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Resíduos vs Ajustados (Modelo Múltiplo)",
       x = "Valores Ajustados",
       y = "Resíduos") +
  theme_minimal(base_size = 14)

# ===================================================================
# Análise Comparativa entre Modelos
# ===================================================================

anova(modelo_simples, modelo_multiplo)

# ===================================================================
# Comentários
# ===================================================================
# - O peso do carro (weight1) tem um efeito negativo significativo no consumo.
# - Ao incluir preço e se o carro é estrangeiro (foreign), o modelo melhora, com R² mais alto.
# - Gráficos diagnósticos indicam alguma heteroscedasticidade leve.
# - O QQ-Plot mostra resíduos próximos da normalidade, sem desvios drásticos.