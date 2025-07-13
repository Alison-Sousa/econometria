# --- Limpar ambiente e carregar pacotes necessários ---
rm(list = ls())

# Definir diretório onde está o dataset
setwd("C:/Users/PC GAMER/Downloads/data")

# Instalar e carregar pacotes necessários
packages <- c("tidyverse", "stargazer", "magrittr", "margins", "caret", "ggplot2")
for(p in packages){
  if(!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# --- Carregar base de dados IFW.csv ---
IFW <- read.csv("IFW.csv")

# Ver estrutura e primeiras linhas do dataset
glimpse(IFW)
head(IFW)

# Selecionar variáveis relevantes para análise
dados <- IFW %>% select(inlf, nwifeinc, educ, exper, age, kidslt6)
summary(dados)
stargazer(dados, type = "text")

# --- Modelo Linear de Probabilidade (LPM) ---
LPM <- lm(inlf ~ nwifeinc + educ + exper + age + kidslt6, data = dados)
summary(LPM)

# --- Modelo Probit ---
Probit <- glm(inlf ~ nwifeinc + educ + exper + age + kidslt6, family = binomial(link = "probit"), data = dados)
summary(Probit)

# --- Modelo Logit ---
Logit <- glm(inlf ~ nwifeinc + educ + exper + age + kidslt6, family = binomial(link = "logit"), data = dados)
summary(Logit)

# --- Adicionar colunas com probabilidades previstas ---
dados <- dados %>%
  mutate(inlfhat_lpm = fitted(LPM),
         inlfhat_probit = fitted(Probit),
         inlfhat_logit = fitted(Logit))

# Exibir primeiras previsões
head(dados %>% select(inlf, inlfhat_lpm, inlfhat_probit, inlfhat_logit))

# --- Gráfico comparativo entre modelos de previsão ---
ggplot(dados, aes(x = inlfhat_probit, y = inlfhat_logit)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(title = "Probabilidades Previstas: Probit vs Logit", x = "Probit", y = "Logit") +
  theme_minimal()

# --- Efeitos marginais ---

# LPM: coeficientes já são os efeitos marginais
summary(LPM)

# Cálculo da média das variáveis independentes
mean_vars <- model.frame(Probit) %>% map_df(mean)

# Probit - efeito marginal na média
Probit.atmean <- margins(Probit, at = mean_vars)
summary(Probit.atmean)

# Probit - efeito marginal médio
Probit.ame <- margins(Probit)
summary(Probit.ame)

# Logit - efeito marginal na média
Logit.atmean <- margins(Logit, at = mean_vars)
summary(Logit.atmean)

# Logit - efeito marginal médio
Logit.ame <- margins(Logit)
summary(Logit.ame)

# --- Pseudo R² (McFadden) ---

# Log-likelihood do modelo irrestrito
LL_ur <- logLik(Probit)

# Modelo restrito (apenas constante)
modelo_restrito <- glm(inlf ~ 1, family = binomial(link = "probit"), data = dados)
LL_0 <- logLik(modelo_restrito)

# Cálculo do pseudo R²
pseudo_r2 <- 1 - as.numeric(LL_ur) / as.numeric(LL_0)
cat("Pseudo R² (McFadden):", round(pseudo_r2, 4), "\n")

# --- Percentual de acertos: Matriz de confusão ---

# Previsões binárias
pred_probit <- as.factor(as.numeric(fitted(Probit) > 0.5))
pred_logit <- as.factor(as.numeric(fitted(Logit) > 0.5))
real <- as.factor(dados$inlf)

# Avaliação preditiva
confusionMatrix(pred_probit, real, positive = "1")
confusionMatrix(pred_logit, real, positive = "1")


# --- Histograma das probabilidades previstas ---
df_preds <- dados %>%
  pivot_longer(cols = starts_with("inlfhat_"), names_to = "modelo", values_to = "prob")

ggplot(df_preds, aes(x = prob, fill = modelo)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Distribuição das Probabilidades Previstas", x = "Probabilidade", y = "Frequência") +
  theme_minimal()
