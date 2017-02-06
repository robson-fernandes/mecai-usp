# Load package
library(networkD3)

# Create fake data
src <- c("Saida de Produtos", "Saida de Produtos","Dia Semana", "Dia Semana")
target <- c("Dia Semana", "Clima", "Feriado", "Salario")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData,
              zoom=T)


# Subs??dio
src <- c("Subsidio", "Colheito","Custo")
target <- c("Custo", "Custo", "Compra")
networkDataCusto <- data.frame(src, target)

# Plot
simpleNetwork(networkDataCusto,
              zoom=T)
