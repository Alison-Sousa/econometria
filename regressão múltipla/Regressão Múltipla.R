# --- Limpar ambiente ---
rm(list = ls())

# --- Definir diretório ---
setwd("C:/Users/PC GAMER/Downloads/data")

# --- Instalar e carregar pacotes necessários ---
pacotes <- c("tidyverse", "stargazer", "magrittr", "car", "ggplot2", "GGally", "gridExtra", "MASS")
for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# --- Carregar bases ---
base1 <- read.csv("base1.csv") # Dados de salários 
base2 <- read.csv("base2.csv") # Salários de CEOs
base3 <- read.csv("base3.csv") # Dados com variável de habilidade
base4 <- read.csv("base4.csv") # Dados educacionais 

# ===========================================================
# 1. REGRESSÃO MÚLTIPLA (base1)
# ===========================================================

# Estrutura e sumário básico
glimpse(base1)
summary(base1)

# Matriz de dispersão para variáveis principais
ggpairs(base1 %>% select(wage, educ, exper, tenure),
        title = "Matriz de Dispersão e Correlação entre Variáveis do Wage",
        lower = list(continuous = "smooth"),
        upper = list(continuous = "cor"))

# Modelo múltiplo completo
modelo_multiplo2 <- lm(wage ~ educ + exper + tenure, data = base1)
summary(modelo_multiplo2)

# Gráficos de diagnóstico clássicos para o modelo múltiplo
par(mfrow = c(2, 2))
plot(modelo_multiplo2, main = "Diagnóstico do Modelo de Regressão Múltipla")
par(mfrow = c(1, 1))

# Resíduos padronizados vs preditos
base1 <- base1 %>% mutate(
  wagehat = fitted(modelo_multiplo2),
  uhat = residuals(modelo_multiplo2),
  std_resid = rstandard(modelo_multiplo2)
)

ggplot(base1, aes(x = wagehat, y = std_resid)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", col = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
  labs(title = "Resíduos Padronizados vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Resíduos Padronizados") +
  theme_minimal(base_size = 14)

# Histograma dos resíduos para checar normalidade
ggplot(base1, aes(x = uhat)) +
  geom_histogram(aes(y=..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma e Densidade dos Resíduos",
       x = "Resíduos",
       y = "Densidade") +
  theme_minimal(base_size = 14)

# QQ-plot dos resíduos para verificar normalidade
qqPlot(modelo_multiplo2, main = "QQ-Plot dos Resíduos")

# ===========================================================
# 2. Salários CEOs (base2)
# ===========================================================

# Visualizar estrutura
glimpse(base2)

# Matriz de dispersão para variáveis chave
ggpairs(base2 %>% select(salary, lsalary, roe, sales, lsales),
        title = "Matriz de Dispersão - CEO Salary",
        lower = list(continuous = "smooth"),
        upper = list(continuous = "cor"))

# Modelos comparativos
modelos_ceo <- list(
  linear = lm(salary ~ roe + sales, data = base2),
  linear_log = lm(salary ~ roe + lsales, data = base2),
  log_linear = lm(lsalary ~ roe + sales, data = base2),
  log_log = lm(lsalary ~ roe + lsales, data = base2)
)

lapply(modelos_ceo, summary)

# Gráfico de resíduos para o melhor modelo (log-log) por exemplo
modelo_ref <- modelos_ceo$log_log
base2 <- base2 %>% mutate(
  salary_hat = fitted(modelo_ref),
  resid = residuals(modelo_ref)
)

ggplot(base2, aes(x = salary_hat, y = resid)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Valores Ajustados (Modelo Log-Log CEO Salary)",
       x = "Valores Ajustados",
       y = "Resíduos") +
  theme_minimal(base_size = 14)

# ===========================================================
# 3. Colinearidade perfeita (base1)
# ===========================================================

# Criar variável male = 1 - female (supondo que exista female)
if("female" %in% colnames(base1)) {
  
  # Garantir que female é numérica 0/1
  if(!is.numeric(base1$female)) {
    base1 <- base1 %>%
      mutate(female = as.numeric(as.character(female)))
  }
  
  if(any(is.na(base1$female))){
    stop("A variável 'female' contém NAs após conversão para numérico.")
  }
  
  base1 <- base1 %>% mutate(male = 1 - female)
  
  modelo_no_col_fem <- lm(wage ~ educ + female, data = base1)
  modelo_no_col_male <- lm(wage ~ educ + male, data = base1)
  
  # Modelos com colinearidade perfeita - não use stargazer neles
  modelo_colinearidade <- lm(wage ~ educ + female + male, data = base1)
  modelo_no_intercepto <- lm(wage ~ 0 + educ + female + male, data = base1)
  
  cat("\nResumo modelo sem colinearidade (educ + female):\n")
  print(summary(modelo_no_col_fem))
  
  cat("\nResumo modelo sem colinearidade (educ + male):\n")
  print(summary(modelo_no_col_male))
  
  cat("\nModelos com colinearidade perfeita (não exibidos pelo stargazer):\n")
  cat("Tente usar summary() para esses modelos individualmente se desejar.\n")
  
  # Exibir modelos sem colinearidade com stargazer
  stargazer(modelo_no_col_fem, modelo_no_col_male, type = "text",
            title = "Modelos de Colinearidade (sem problemas)")
  
} else {
  cat("Variável 'female' não encontrada em base1. Pulando seção de colinearidade perfeita.\n")
}

# ===========================================================
# 4. Multicolinearidade (base4)
# ===========================================================

base4_sel <- base4 %>% select(api00, avg_ed, grad_sch, col_grad) %>% na.omit()

# Matriz de correlação gráfica para detectar multicolinearidade
ggpairs(base4_sel %>% select(-api00), 
        title = "Matriz de Correlação entre Variáveis Explicativas (base4)")

# VIF - multicolinearidade
modelo_vif_alto <- lm(api00 ~ avg_ed + grad_sch + col_grad, data = base4_sel)
vifs <- vif(modelo_vif_alto)
print(vifs)

# Gráfico dos VIFs
vif_df <- data.frame(Variavel = names(vifs), VIF = vifs)
ggplot(vif_df, aes(x = reorder(Variavel, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Valores do VIF para Variáveis (base4)", y = "VIF", x = "Variável") +
  theme_minimal(base_size = 14)

# Ajustar modelo removendo variável com VIF alto (se houver)
modelo_vif_baixo <- lm(api00 ~ grad_sch + col_grad, data = base4_sel)
summary(modelo_vif_baixo)

# ===========================================================
# 5. Viés por variável omitida (base3)
# ===========================================================

base3_sel <- base3 %>% select(wage, educ, abil) %>% na.omit()

# Modelo verdadeiro
modelo_verdadeiro <- lm(wage ~ educ + abil, data = base3_sel)
summary(modelo_verdadeiro)

# Modelo para abil em função da educ
modelo_abil <- lm(abil ~ educ, data = base3_sel)
summary(modelo_abil)

# Modelo omitido (sem abil)
modelo_omitido <- lm(wage ~ educ, data = base3_sel)
summary(modelo_omitido)

# Viés estimado
beta2 <- coef(modelo_verdadeiro)["abil"]
delta1 <- coef(modelo_abil)["educ"]
bias <- beta2 * delta1
cat("Viés na variável educ estimado: ", round(bias,4), "\n")

# ===========================================================
# 6. Heteroscedasticidade (base1)
# ===========================================================

# Gráficos avançados dos resíduos vs variáveis explicativas

# Residuals vs Educ
p1 <- ggplot(base1, aes(x = educ, y = uhat)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Educação", x = "Educação (anos)", y = "Resíduos") +
  theme_minimal(base_size = 14)

# Residuals vs Exper
p2 <- ggplot(base1, aes(x = exper, y = uhat)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Experiência", x = "Experiência (anos)", y = "Resíduos") +
  theme_minimal(base_size = 14)

# Residuals vs Tenure
p3 <- ggplot(base1, aes(x = tenure, y = uhat)) +
  geom_point(alpha = 0.6, color = "darkcyan") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Tempo de Empresa (Tenure)", x = "Tenure (anos)", y = "Resíduos") +
  theme_minimal(base_size = 14)

# Mostrar os 3 plots juntos para comparação
grid.arrange(p1, p2, p3, ncol = 1)

# ===========================================================
# 7. Gráficos extras para base1 (distribuição das variáveis)
# ===========================================================

# Histogramas das variáveis contínuas
vars_cont <- c("wage", "educ", "exper", "tenure")
plots_hist <- lapply(vars_cont, function(var){
  ggplot(base1, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    labs(title = paste("Histograma de", var), x = var, y = "Frequência") +
    theme_minimal(base_size = 14)
})
grid.arrange(grobs = plots_hist, ncol = 2)

# Boxplots para detectar outliers
plots_box <- lapply(vars_cont, function(var){
  ggplot(base1, aes_string(y = var)) +
    geom_boxplot(fill = "tomato", alpha = 0.7) +
    labs(title = paste("Boxplot de", var), y = var) +
    theme_minimal(base_size = 14)
})
grid.arrange(grobs = plots_box, ncol = 2)
