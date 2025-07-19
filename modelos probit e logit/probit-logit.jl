using CSV, DataFrames, GLM, StatsModels, Plots

# 1. Carregar dados
df = CSV.read("C:/Users/PC GAMER/Downloads/data/IFW.csv", DataFrame)

# 2. Selecionar variáveis
select!(df, [:inlf, :nwifeinc, :educ, :exper, :age, :kidslt6])

# 3. Ajustar modelos
form = @formula(inlf ~ nwifeinc + educ + exper + age + kidslt6)

lpm    = lm(form, df)
probit = glm(form, df, Binomial(), ProbitLink())
logit  = glm(form, df, Binomial(), LogitLink())

# 4. Calcular probabilidades previstas
df.lpm    = predict(lpm)
df.probit = predict(probit)
df.logit  = predict(logit)

# 5. Empilhar dados (long format)
df_long = DataFrame()
append!(df_long, DataFrame(prob = df.lpm, model = "LPM"))
append!(df_long, DataFrame(prob = df.probit, model = "Probit"))
append!(df_long, DataFrame(prob = df.logit, model = "Logit"))

# 6. Plotar histograma comparativo
p = histogram(df_long.prob,
    group=df_long.model,
    bins=30,
    legend=:topright,
    alpha=0.6,
    lw=0.5,
    title="Distribuição das Probabilidades Previstas",
    xlabel="Probabilidade",
    ylabel="Frequência",
    color=[:darkorange :steelblue :forestgreen],
    framestyle=:box,
    background_color=:white)

# 7. Salvar como PDF
savefig(p, "distribuicao_probabilidades.pdf")
