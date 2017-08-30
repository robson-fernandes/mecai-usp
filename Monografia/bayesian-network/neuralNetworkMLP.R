# ------------------------------------------
# Rede Neural MLP - 2 camadas ocultas (3,2) neurônios
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

library(plotly)
library(neuralnet)
library(forecast)
library(StatMeasures)
library(LPAREN)

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
# Funcao - Ajuste Modelo - Rede Neural MLP
#
fitModelNeuralNetworkMLP <- function(training.set)
{
  formula <- as.formula('venda  ~ 
                          grupoMilkShake + 
                          grupoSanduiche + 
                          grupoBebida + 
                          grupoAcompanhamento + 
                          grupoPrato + 
                          grupoAdicional + 
                          grupoBrinde + 
                          quantidadeProduto   + 
                          mes')
  
  fit.neural.network = neuralnet(formula,
                    data=training.set,
                    linear.output=TRUE,
                    hidden=c(3,2),
                    threshold =0.01,
                    rep=10,
                    algorithm = "rprop+")
  
  # if (file.exists("fitNN.rds")) 
  # {
  #   fit.neural.network <- readRDS('fitNN.rds')
  # }
  # else 
  # {
  #   fit.neural.network = neuralnet(formula,
  #                   data=data,
  #                   linear.output=TRUE,
  #                   hidden=c(3,2),
  #                   threshold =0.01,
  #                   rep=2)# algorithm = "rprop+"
  #   
  #   saveRDS(fit.neural.network, "fitNN.rds")
  # }

  plot(fit.neural.network,
       col.entry="green",
       col.hidden="blue",
       col.out="red",
       rep="best")
  
  return(fit.neural.network)
}


#
# Funcao - Predição - Rede Neural MLP
#
predictNeuralNetworkMLP <- function(fit.neural.network, test.set)
{
  predict.neural.network <- compute(fit.neural.network, test.set)
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
plotNeuralNetworkMLP <- function(ds.resultado)
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
              line = list(color = 'rgb(0, 193, 9)', width = 3),
              name = "Modelo Rede Neural MLP", 
              connectgaps = TRUE)
  p
}


# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Rede Neural MLP
fit.neural.network <- fitModelNeuralNetworkMLP(training.set)

#Predição - Rede Neural MLP
predict.neural.network <- predictNeuralNetworkMLP(fit.neural.network, test.set[1:9])

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.neural.network$net.result
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Médio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotNeuralNetworkMLP(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]

dif <- previsto - real
sum(previsto)
sum(real)