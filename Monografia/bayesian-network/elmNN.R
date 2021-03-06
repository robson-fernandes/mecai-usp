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
  dados.grupos <- read.table("dados-grupos.csv", 
                             header=TRUE, 
                             sep=";")
  attach(dados.grupos)
  
  dados <-  cbind(dados.grupos$mes, 
                  dados.grupos$quantidadeProduto,
                  dados.grupos$grupoMilkShake, 
                  dados.grupos$grupoSanduiche, 
                  dados.grupos$grupoBebida,
                  dados.grupos$grupoAcompanhamento,
                  dados.grupos$grupoPrato,
                  dados.grupos$grupoAdicional,
                  dados.grupos$grupoBrinde,
                  dados.grupos$venda)
  
  colnames(dados) <- c("mes", 
                       "quantidadeProduto", 
                       "grupoMilkShake", 
                       "grupoSanduiche",
                       "grupoBebida",
                       "grupoAcompanhamento",
                       "grupoPrato",
                       "grupoAdicional",
                       "grupoBrinde",
                       "venda")
  
  #
  # Conjunto de Treinamento
  #
  training.setOriginal <<- dados[1:85, ]
  training.set <- training.setOriginal
  training.set[,"mes"] <- as.double(training.set[,"mes"])
  training.set <- scale(training.set)
  training.set <<- as.data.frame(training.set)
  
  #
  # Conjunto de Teste
  #
  test.setOriginal <<- dados[85:90, ]
  test.set <- test.setOriginal
  test.set[,"mes"] <- as.double(test.set[,"mes"])
  test.set <- scale(test.set)
  test.set <<- as.data.frame(test.set)
}

#
# Funcao - Ajuste Modelo - Rede Neural ELM
#
fitModelNeuralNetworkELM <- function(training.set)
{
  fit.neural.network <- elmtrain(venda ~ 
                        mes +
                        quantidadeProduto  +
                        grupoMilkShake + 
                        grupoSanduiche + 
                        grupoBebida +
                        grupoAcompanhamento +
                        grupoPrato + 
                        grupoAdicional, 
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
getDataSet.RealvsPrevisto <- function(real, previsto)
{
  previsto.fit = scaleToOriginal(real,previsto)
  data <- seq(as.Date("2017/1/1"), by = "month", length.out = 6)
  periodo <- as.Date(data , "%m/%d/%y")
  result = cbind.data.frame(real, previsto.fit, periodo)
  colnames(result) = c("real", "previsto","periodo")
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
  x <- list( title = "Período", titlefont = f)
  y <- list(title = "Venda (R$)", titlefont = f)
  
  p <- plot_ly(as.data.frame(ds.resultado), 
               x = ~periodo,
               y = ~real, 
               name = "Real", 
               type = "scatter",
               mode = "lines") %>%
    layout(xaxis = x, yaxis = y)  %>%
    add_trace(y = ~previsto, 
              line = list(color = 'rgb(121, 85, 72)', width = 3),
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
real <- test.setOriginal[,"venda"]
previsto <- predict.neural.network
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Médio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotNeuralNetworkELM(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]

dif <- previsto - real
sum(previsto)
sum(real)