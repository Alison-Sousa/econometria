# INFERÊNCIA EM REGRESSÃO LINEAR - R 4.5.1
# Autor: Alison Cordeiro Sousa
# Versão adaptada com foco em inferência causal

# Limpar o ambiente
rm(list = ls())

# Definir diretório onde o dataset está localizado
setwd("C:/Users/PC GAMER/Downloads/data")

# Carregar bibliotecas (assumindo que já estão instaladas)
library(tidyverse)
library(stargazer)
library(magrittr)
library(car)

# Carregar base de dados
wage1 <- read.csv("wage1.csv")

# Visualização inicial da base
wage1 %>% select(wage, educ, exper, tenure, female) %>% head(10)
wage1 %>% select(wage, educ, exper, tenure, female) %>% str()
wage1 %>% select(wage, educ, exper, tenure, female) %>% stargazer(type = "text")

# ------------------------------------------------------------------------------
# 1. Verificação de Normalidade - Teste de Shapiro-Wilk
# ------------------------------------------------------------------------------
ggplot(data = wage1) +
  theme_minimal() +
  geom_histogram(aes(x = wage), bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribuição da Variável: wage")

# Criar variável log(wage) para comparação
wage1$lwage <- log(wage1$wage)

ggplot(data = wage1) +
  theme_minimal() +
  geom_histogram(aes(x = lwage), bins = 30, fill = "tomato", color = "black") +
  labs(title = "Distribuição da Variável: log(wage)")

# Teste de normalidade
shapiro.test(wage1$wage)
shapiro.test(wage1$lwage)

# ------------------------------------------------------------------------------
# 2. Estimação do Modelo de Regressão Linear
# ------------------------------------------------------------------------------
modelo <- lm(wage ~ educ + exper + tenure + female, data = wage1)
summary(modelo)

# ------------------------------------------------------------------------------
# 3. Teste t para Significância de Coeficientes
# ------------------------------------------------------------------------------
coef_exper <- coef(modelo)["exper"]
se_exper <- sqrt(vcov(modelo)["exper", "exper"])
tstat <- coef_exper / se_exper
gl_resid <- modelo$df.residual

# Valor crítico t (nível de 5%)
tcrit <- qt(0.975, df = gl_resid)

# Valor-p (bilateral)
p_valor_t_bilateral <- 2 * pt(abs(tstat), df = gl_resid, lower.tail = FALSE)

# Valor-p (unilateral superior)
p_valor_t_superior <- pt(tstat, df = gl_resid, lower.tail = FALSE)

# Intervalos de confiança
IC_95_inf <- coef_exper - 1.96 * se_exper
IC_95_sup <- coef_exper + 1.96 * se_exper

# ------------------------------------------------------------------------------
# 4. Teste F para Significância Individual (exper)
# ------------------------------------------------------------------------------
modelo_restrito_1 <- lm(wage ~ educ + tenure + female, data = wage1)
ssr_r1 <- sum(resid(modelo_restrito_1)^2)
ssr_ur <- sum(resid(modelo)^2)
q <- 1

F_stat <- ((ssr_r1 - ssr_ur)/q) / (ssr_ur / gl_resid)
F_crit <- qf(0.95, df1 = q, df2 = gl_resid)
p_valor_F <- pf(F_stat, df1 = q, df2 = gl_resid, lower.tail = FALSE)

# Verificação usando car::linearHypothesis
linearHypothesis(modelo, "exper = 0")

# ------------------------------------------------------------------------------
# 5. Teste F para Significância Conjunta (exper e tenure)
# ------------------------------------------------------------------------------
modelo_restrito_2 <- lm(wage ~ educ + female, data = wage1)
ssr_r2 <- sum(resid(modelo_restrito_2)^2)
q <- 2

F_stat_joint <- ((ssr_r2 - ssr_ur)/q) / (ssr_ur / gl_resid)
F_crit_joint <- qf(0.95, df1 = q, df2 = gl_resid)
p_valor_joint <- pf(F_stat_joint, df1 = q, df2 = gl_resid, lower.tail = FALSE)

# Verificação com linearHypothesis
linearHypothesis(modelo, c("exper = 0", "tenure = 0"))

# ------------------------------------------------------------------------------
# 6. Teste F para Significância Global do Modelo
# ------------------------------------------------------------------------------
modelo_restrito_3 <- lm(wage ~ 1, data = wage1)
ssr_r3 <- sum(resid(modelo_restrito_3)^2)
q <- 4

F_stat_global <- ((ssr_r3 - ssr_ur)/q) / (ssr_ur / gl_resid)
R2 <- summary(modelo)$r.squared
k <- modelo$rank - 1
F_stat_alt <- (R2 / k) / ((1 - R2) / gl_resid)
F_crit_global <- qf(0.95, df1 = q, df2 = gl_resid)
p_valor_global <- pf(F_stat_global, df1 = q, df2 = gl_resid, lower.tail = FALSE)

linearHypothesis(modelo, c("educ = 0", "exper = 0", "tenure = 0", "female = 0"))

# ------------------------------------------------------------------------------
# 7. Teste de Multiplicadores de Lagrange (LM Test) para Significância Conjunta
# ------------------------------------------------------------------------------
wage1$ehat <- residuals(modelo_restrito_2)
modelo_ehat <- lm(ehat ~ educ + exper + tenure + female, data = wage1)

R2_ehat <- summary(modelo_ehat)$r.squared
n <- nobs(modelo_ehat)
LM_stat <- n * R2_ehat
q <- 2

# Valor crítico qui-quadrado
chi2_crit <- qchisq(0.95, df = q)
p_valor_chi2 <- pchisq(LM_stat, df = q, lower.tail = FALSE)

# ------------------------------------------------------------------------------
# 8. Gráfico de Diagnóstico para Inferência Causal
# ------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(modelo)

# Diagnóstico adicional de influência
library(ggfortify)
autoplot(modelo)

# ------------------------------------------------------------------------------
# Conclusão
# ------------------------------------------------------------------------------
cat("Resumo Final:\n")
cat("T-Stat exper =", round(tstat, 3), "| P-valor (bilateral) =", round(p_valor_t_bilateral, 4), "\n")
cat("F-Stat (exper) =", round(F_stat, 3), "| P-valor =", round(p_valor_F, 4), "\n")
cat("F-Stat (exper + tenure) =", round(F_stat_joint, 3), "| P-valor =", round(p_valor_joint, 4), "\n")
cat("F-Stat Global =", round(F_stat_global, 3), "| P-valor =", round(p_valor_global, 4), "\n")
cat("LM Stat =", round(LM_stat, 3), "| P-valor Qui² =", round(p_valor_chi2, 4), "\n")

