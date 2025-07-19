# ===================================================================
# CONFIGURAÇÃO INICIAL E CARREGAMENTO DOS DADOS
# ===================================================================
using CSV
using DataFrames
using GLM
using StatsPlots
using Plots
gr()  # usa o backend GR (mais compatível para exportar PDF)

# Define o diretório de trabalho (ajuste conforme necessário)
cd("C:/Users/PC GAMER/Downloads/data")

# Carrega o CSV
auto = CSV.read("auto.csv", DataFrame)

# ===================================================================
# MODELO DE REGRESSÃO MÚLTIPLA
# ===================================================================
modelo_multiplo = lm(@formula(mpg ~ weight1 + price + foreign), auto)

# Previsões e resíduos
auto.yhat = predict(modelo_multiplo)
auto.resid = residuals(modelo_multiplo)

# ===================================================================
# 1. GRÁFICO DE DISPERSÃO: mpg vs weight1 + linha de tendência
# ===================================================================
scatter1 = @df auto scatter(:weight1, :mpg,
    title = "Relação entre Peso do Veículo e Consumo",
    xlabel = "Peso do veículo (weight1)",
    ylabel = "Milhas por galão (mpg)",
    label = "Dados",
    color = :blue,
    alpha = 0.6,
    legend = :topright)

# Ajuste linear
modelo_simples = lm(@formula(mpg ~ weight1), auto)
w = auto.weight1
mpg_pred = predict(modelo_simples)
sort_idx = sortperm(w)
plot!(w[sort_idx], mpg_pred[sort_idx],
    label = "Ajuste Linear", color = :red, lw = 2)

# Salvar como PDF
savefig(scatter1, "grafico_dispersao.pdf")

# ===================================================================
# 2. RESÍDUOS vs VALORES AJUSTADOS (Modelo Múltiplo)
# ===================================================================
resplot = @df auto scatter(:yhat, :resid,
    title = "Resíduos vs Valores Ajustados",
    xlabel = "Valores Ajustados",
    ylabel = "Resíduos",
    label = "",
    color = :purple,
    alpha = 0.7)
hline!([0], linestyle = :dash, color = :black, label = "")

# Salvar como PDF
savefig(resplot, "residuos_ajustados.pdf")
