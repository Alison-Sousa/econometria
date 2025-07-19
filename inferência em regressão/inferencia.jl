using CSV, DataFrames, GLM, Plots

# Carregar dados
df = CSV.read("C:/Users/PC GAMER/Downloads/data/wage1.csv", DataFrame)

# Modelo
modelo = lm(@formula(wage ~ educ + exper + tenure + female), df)

# Preditos e resíduos
ŷ = predict(modelo)
e = residuals(modelo)

# Gráfico de resíduos vs valores ajustados
p = scatter(ŷ, e,
    title = "Resíduos vs Valores Ajustados",
    xlabel = "Valores Ajustados",
    ylabel = "Resíduos",
    legend = false,
    color = :darkblue,
    markersize = 3,
    grid = true)

# Salvar em PDF
savefig(p, "grafico_residuos.pdf")
