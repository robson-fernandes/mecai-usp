# ------------------------------------------
# Rede Neural SLFN - ELM - Máquina de Aprendizado Profundo
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

library(elmNN)
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
# Funcao - Ajuste Modelo - Rede Neural ELM
#
fitModelNeuralNetworkELM <- function(training.set)
{
  fit.neural.network <- elmtrain(vendas ~ 
                                   anoExp +
                                   scoreIntel, 
                                   data=training.set, 
                                   nhid=10, 
                                   actfun="purelin")
  
  return(fit.neural.network)
}

#
# Funcao - Predição - Rede Neural ELM
#
predictNeuralNetworkELM <- function(fit.neural.network, test.set)
{
  predict.neural.network <- predict(fit.neural.network,newdata=test.set)
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
plotNeuralNetworkELM <- function(ds.resultado)
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
              name = "Modelo Rede Neural SLFN - ELM", 
              connectgaps = TRUE)
  p
}


# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Rede Neural ELM
fit.neural.network <- fitModelNeuralNetworkELM(training.set)

#Predição - Rede Neural ELM
predict.neural.network <- predictNeuralNetworkELM(fit.neural.network, test.set)

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"vendas"]
vendedor <- test.setOriginal[,"vendedor"]
previsto <- predict.neural.network
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto,vendedor)

#Erro Percentual Absoluto Médio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotNeuralNetworkELM(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]