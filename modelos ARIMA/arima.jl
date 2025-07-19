using ARFIMA, CSV, DataFrames, Plots, Statistics

# 1. Carregar os dados corretamente
function load_data()
    try
        df = CSV.read("timeseries.csv", DataFrame)
        if :ppi in propertynames(df)
            return df[:, :ppi]
        else
            error("Coluna 'ppi' não encontrada no arquivo CSV")
        end
    catch e
        error("Erro ao carregar arquivo: ", e)
    end
end

Y = load_data()

# 2. Análise estatística básica
println("\nEstatísticas descritivas:")
println("Média: ", mean(Y))
println("Desvio padrão: ", std(Y))
println("Mínimo: ", minimum(Y))
println("Máximo: ", maximum(Y))

# 3. Simulação ARIMA com parâmetros baseados nos dados
N = length(Y)
σ = std(Y)  # Agora funcionará pois importamos Statistics

# Parâmetros ARIMA(1,1,1) - ajuste conforme necessário
ar_param = 0.5    # Parâmetro AR(1)
ma_param = 0.3    # Parâmetro MA(1)
d_order = 1       # Ordem de diferenciação

# Simulação do processo ARIMA
simulated = arfima(N, σ, d_order, SVector(ar_param), SVector(ma_param))

# 4. Visualização comparativa
p = plot(layout=(2,1), size=(900,600), dpi=300)

# Subplot 1: Dados originais
plot!(p[1], Y, 
      label="Dados Originais", 
      linewidth=2, 
      color=:blue,
      title="Dados Reais vs ARIMA(1,1,1) Simulado")

# Subplot 2: Dados simulados
plot!(p[2], simulated, 
      label="ARIMA Simulado", 
      linewidth=2, 
      color=:red,
      xlabel="Períodos")

# 5. Salvar resultados
savefig(p, "analise_arima_comparacao.pdf")
println("\nAnálise concluída. Gráfico salvo como 'analise_arima_comparacao.pdf'")

# 6. Função adicional para análise de resíduos (exemplo)
function analyze_residuals(data)
    residuals = diff(data)  # Resíduos simples
    p_resid = plot(residuals, label="Resíduos", color=:purple)
    savefig(p_resid, "residuos_arima.pdf")
end

analyze_residuals(Y)