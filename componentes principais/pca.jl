using MultivariateStats, Plots, DataFrames, CSV, Statistics

# 1. Carregar os dados
function carregar_dados(caminho)
    try
        dados = CSV.read(caminho, DataFrame)
        # Selecionar apenas colunas numéricas
        colunas_numericas = [col for col in names(dados) if eltype(dados[!, col]) <: Number]
        return dados[:, colunas_numericas]
    catch e
        error("Erro ao carregar dados: ", e)
    end
end

dados_num = carregar_dados("pca.csv")

# 2. Realizar PCA
function realizar_pca(dados)
    # Remover valores faltantes (PCA não lida com missing values)
    dados_completos = dropmissing(dados)
    # Padronizar os dados (importante para PCA)
    dados_padronizados = Matrix(dados_completos)
    dados_padronizados = (dados_padronizados .- mean(dados_padronizados, dims=1)) ./ std(dados_padronizados, dims=1)
    # Ajustar modelo PCA
    pca = fit(PCA, dados_padronizados'; maxoutdim=min(size(dados_padronizados)...))
    return pca
end

pca_model = realizar_pca(dados_num)

# 3. Extrair variância explicada
variancia_explicada = principalvars(pca_model) ./ tvar(pca_model)

# 4. Criar gráfico Scree Plot (o mais relevante)
function criar_scree_plot(variancia)
    p = plot(1:length(variancia), variancia, 
        xticks=1:length(variancia),
        title="Scree Plot - Variância Explicada por Componente",
        xlabel="Número do Componente Principal",
        ylabel="Proporção da Variância Explicada",
        legend=false,
        markershape=:circle,
        markercolor=:blue,
        markersize=5,
        linewidth=2,
        color=:blue,
        dpi=300)
    
    # Adicionar linha de referência (critério de Kaiser)
    hline!([1/length(variancia)], linestyle=:dash, color=:red, label="Critério de Kaiser")
    
    # Adicionar valores percentuais
    for i in 1:length(variancia)
        annotate!(i, variancia[i], text("$(round(variancia[i]*100, digits=1))%", 8, :top))
    end
    
    return p
end

scree_plot = criar_scree_plot(variancia_explicada)

# 5. Salvar gráfico
savefig(scree_plot, "scree_plot_pca.pdf")