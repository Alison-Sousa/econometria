# Carregar pacotes
using GLM, Plots, DataFrames, CSV

# 1. Carregar dados
dados = CSV.read("base1.csv", DataFrame)

# 2. Ajustar modelo de regressão
modelo = lm(@formula(wage ~ educ + exper + tenure), dados)

# 3. Calcular resíduos e valores ajustados
residuos = residuals(modelo)
valores_ajustados = predict(modelo)

# 4. Criar gráfico PhD de resíduos vs ajustados
grafico = scatter(valores_ajustados, residuos,
    title = "ANÁLISE DE RESÍDUOS",
    xlabel = "Valores Ajustados",
    ylabel = "Resíduos",
    color = :blue,
    legend = false,
    markersize = 5,
    markeralpha = 0.6,
    dpi = 300)

# Adicionar linha de referência em zero
hline!([0], linestyle = :dash, color = :red)

# Adicionar suavização LOESS
using Loess
modelo_loess = loess(valores_ajustados, residuos)
suave_x = range(minimum(valores_ajustados), maximum(valores_ajustados), length=100)
suave_y = predict(modelo_loess, suave_x)
plot!(suave_x, suave_y, linewidth=2, color=:red)

# 5. Salvar em PDF
savefig(grafico, "analise_residuos.pdf")