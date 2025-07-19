using CSV, DataFrames, LinearAlgebra, NearestNeighbors, Plots

# 1. Carregar dados
df = CSV.read("C:/Users/PC GAMER/Downloads/data/data.csv", DataFrame)
coords = hcat(df.X, df.Y)  # Matriz 2 colunas

# 2. Criar estrutura de vizinhança: até distância 10
tree = BallTree(coords', leafsize = 10)  # NearestNeighbors espera linhas = dimensão
dmax = 10.0
neighbors = [findall(i -> i != j && norm(coords[i,:] - coords[j,:]) ≤ dmax, 1:size(coords, 1)) for j in 1:size(coords,1)]

# 3. Construir segmentos (arestas da rede)
segments = Tuple{Float64, Float64, Float64, Float64}[]
for i in 1:length(neighbors)
    for j in neighbors[i]
        push!(segments, (coords[i,1], coords[i,2], coords[j,1], coords[j,2]))
    end
end

# 4. Gerar gráfico
p = plot(; legend=false, title="Rede de Vizinhança Espacial (d ≤ 10)", background_color=:white)
for (x, y, xend, yend) in segments
    plot!([x, xend], [y, yend], lc=:gray60, lw=0.8, alpha=0.6)
end

scatter!(coords[:,1], coords[:,2],
    color=:darkred, markerstrokecolor=:black, markersize=4,
    xlabel="Coordenada X", ylabel="Coordenada Y", framestyle=:box)

# 5. Salvar como PDF
savefig(p, "vizinhanca_espacial.pdf")
