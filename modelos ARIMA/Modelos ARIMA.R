# --- Pacotes necessários ---
library(tseries)    # Testes de raiz unitária
library(forecast)   # Ferramentas avançadas para séries temporais e plotagem
library(ggplot2)    # Gráficos personalizados

# --- Definir diretório de trabalho ---
setwd("C:/Users/PC GAMER/Downloads/data")

# --- Carregar os dados ---
df <- read.csv("timeseries.csv")

# --- Supondo que a variável de interesse se chame "ppi" ---
Y <- df$ppi

# --- Estatísticas descritivas ---
summary(Y)
plot.ts(Y, main="Série Temporal Original (ppi)", ylab="ppi", col="darkblue")

# --- Teste Dickey-Fuller para estacionariedade ---
adf_original <- adf.test(Y, alternative = "stationary")
print(adf_original)

# --- Diferença para estacionarizar (se necessário) ---
dY <- diff(Y)

summary(dY)
plot.ts(dY, main="Série Diferenciada (primeira diferença)", ylab="Diff ppi", col="darkgreen")

adf_diff <- adf.test(dY, alternative = "stationary")
print(adf_diff)

# --- Função para ajustar modelo ARIMA, retornar AIC e objeto ---
ajustar_arima <- function(order_vec, serie){
  fit <- tryCatch(
    arima(serie, order=order_vec),
    error = function(e) NULL
  )
  if(is.null(fit)) return(NULL)
  return(list(model=fit, aic=AIC(fit), order=order_vec))
}

# --- Modelos a testar (p,d,q) ---
modelos_testar <- list(
  c(1,0,0),
  c(2,0,0),
  c(0,0,1),
  c(1,0,1),
  c(1,1,0),
  c(0,1,1),
  c(1,1,1),
  c(1,1,3),
  c(2,1,3)
)

# --- Ajustar modelos e guardar resultados ---
resultados <- lapply(modelos_testar, ajustar_arima, serie=Y)

# --- Filtrar modelos que convergiram ---
resultados_validos <- Filter(Negate(is.null), resultados)

# --- Criar tabela resumo com ordem e AIC ---
aic_table <- do.call(rbind, lapply(resultados_validos, function(x){
  data.frame(p=x$order[1], d=x$order[2], q=x$order[3], AIC=x$aic)
}))

print(aic_table[order(aic_table$AIC), ])

# --- Selecionar melhor modelo pelo menor AIC ---
melhor_modelo <- resultados_validos[[which.min(sapply(resultados_validos, function(x) x$aic))]]
cat("Melhor modelo ARIMA selecionado: (p,d,q) = (", melhor_modelo$order, ") com AIC = ", melhor_modelo$aic, "\n")

# --- Resumo do melhor modelo ---
summary(melhor_modelo$model)

# --- Diagnóstico do melhor modelo ---
par(mfrow=c(2,2))
plot.ts(residuals(melhor_modelo$model), main="Resíduos do modelo ARIMA", col="purple")
acf(residuals(melhor_modelo$model), main="ACF dos resíduos")
pacf(residuals(melhor_modelo$model), main="PACF dos resíduos")
qqnorm(residuals(melhor_modelo$model))
qqline(residuals(melhor_modelo$model))
par(mfrow=c(1,1))

# --- Previsão com o melhor modelo ---
horizonte <- 100  # Previsão para 100 períodos à frente
prev <- predict(melhor_modelo$model, n.ahead=horizonte)

# --- Construir data.frame para gráfico ---
tempo_original <- 1:length(Y)
tempo_prev <- (length(Y)+1):(length(Y)+horizonte)

df_graf <- data.frame(
  Tempo = c(tempo_original, tempo_prev, tempo_prev),
  Valor = c(Y, prev$pred, prev$pred),
  Tipo = factor(c(
    rep("Observado", length(Y)),
    rep("Previsto", horizonte),
    rep("Intervalo Superior e Inferior", horizonte)
  ), levels=c("Observado", "Previsto", "Intervalo Superior e Inferior"))
)

# --- Intervalos de confiança ---
df_ic <- data.frame(
  Tempo = tempo_prev,
  Inferior = prev$pred - 2*prev$se,
  Superior = prev$pred + 2*prev$se
)

# --- Plot da série original e previsão com intervalo ---
ggplot() +
  geom_line(data = data.frame(Tempo = tempo_original, Valor = Y), aes(x=Tempo, y=Valor), color="darkblue") +
  geom_line(data = data.frame(Tempo = tempo_prev, Valor = prev$pred), aes(x=Tempo, y=Valor), color="red") +
  geom_ribbon(data = df_ic, aes(x=Tempo, ymin=Inferior, ymax=Superior), alpha=0.3, fill="pink") +
  labs(title = paste0("Previsão ARIMA(", paste(melhor_modelo$order, collapse=","), ") para ppi"),
       x = "Tempo", y = "ppi") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold"))

