# ------------------------------------------
# Rede Neural MLP - Backpropagation Resiliente
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

library(neuralnet)
library(plotly)
library(StatMeasures)

#
# Funcao - Carregar conjunto de Treinamento e Teste
#
loadDataSet <- function()
{
  dados.vendedor <- read.table("vendedor.csv", 
                               header=TRUE, 
                               sep=";")
  attach(dados.vendedor)
  
  dados <-  cbind(dados.vendedor$vendedor,
                  dados.vendedor$anoExp, 
                  dados.vendedor$scoreIntel,
                  dados.vendedor$vendas)
  
  colnames(dados) <- c("vendedor",
                       "anoExp", 
                       "scoreIntel",
                       "vendas")
  
  #
  # Conjunto de Treinamento
  #
  training.setOriginal <<- dados[1:8, ]
  training.set <- training.setOriginal
  training.set <- scale(training.set)
  training.set <<- as.data.frame(training.set)
  
  #
  # Conjunto de Teste
  #
  test.setOriginal <<- dados[8:12, ]
  test.set <- test.setOriginal
  test.set <- scale(test.set)
  test.set <<- as.data.frame(test.set)
}

#
# Funcao - Ajuste Modelo - Rede Neural MLP - BackPropagation
#
fitModelNeuralNetworkMLP <- function(training.set)
{
  formula <- as.formula('vendas  ~ 
                          anoExp +
                          scoreIntel')
  
  fit.neural.network = neuralnet(formula,
                                 data=training.set,
                                 linear.output=TRUE,
                                 hidden=c(3,2),
                                 threshold =0.01,
                                 rep=10,
                                 algorithm = "rprop+")
  
  plot(fit.neural.network,
       col.entry="green",
       col.hidden="blue",
       col.out="red",
       rep="best")
  
  return(fit.neural.network)
}

#
# Funcao - Predição - Rede Neural MLP - BackPropagation
#
predictNeuralNetworkMLP <- function(fit.neural.network, test.set)
{
  predict.neural.network <- compute(fit.neural.network,test.set)
  return(predict.neural.network)
}

#
# Funcao - Conversão de valores normalizados em escala para original
#
scaleToOriginal <- function(value, scale.value)
{
  s <- scale.value
  y.sd = sd(value)
  y.mean = mean(value)
  original.value = s * y.sd + y.mean
  
  return(original.value)
}

#
# Funcao - Coletar conjunto de Dados - Real vs Previsto
#
getDataSet.RealvsPrevisto <- function(real, previsto,vendedor)
{
  previsto.fit = scaleToOriginal(real,previsto)
  result = cbind.data.frame(vendedor, real, previsto.fit)
  colnames(result) = c("vendedor", "real", "previsto")
  return(result)
}

#
# Funcao - Erro Percentual Absoluto Médio
#
getMape <- function(data.set)
{
  mape <- mape(y = data.set[, "real"], 
               yhat = data.set[, "previsto"])
  return(mape*100)
}


#
# Funcao - Visualizar gráfico do modelo
#
plotNeuralNetworkMLP <- function(ds.resultado)
{
  f <- list(family = "Verdana", size = 14, color = "#000000")
  x <- list( title = "Vendedor", titlefont = f)
  y <- list(title = "Venda (R$)", titlefont = f)
  
  p <- plot_ly(as.data.frame(ds.resultado), 
               x = ~vendedor,
               y = ~real, 
               name = "Real", 
               type = "scatter",
               mode = "lines") %>%
    layout(xaxis = x, yaxis = y)  %>%
    add_trace(y = ~previsto, 
              line = list(color = 'rgb(255, 87, 34)', width = 3),
              name = "Modelo Rede Neural MLP - BProp", 
              connectgaps = TRUE)
  p
}


# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Rede Neural MLP - BackPropagation
fit.neural.network <- fitModelNeuralNetworkMLP(training.set)

#Predição - Rede Neural  MLP - BackPropagation
predict.neural.network <- predictNeuralNetworkMLP(fit.neural.network, test.set[2:3])

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"vendas"]
vendedor <- test.setOriginal[,"vendedor"]
previsto <- predict.neural.network$net.result
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto,vendedor)

#Erro Percentual Absoluto Médio
getMape(ds.resultado)

#Visualizar Gráfico do Modelo
plotNeuralNetworkMLP(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]