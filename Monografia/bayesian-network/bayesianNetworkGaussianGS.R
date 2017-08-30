# ------------------------------------------
# Rede Bayesiana Gaussiana - GS
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

#
# Pacotes
#
# <bnlearn>
# Pacote de Análise e Inferência em Redes Bayesianas
#
# <StatMeasures>
# Pacote de verificações estatísticas
#
# <plotly>
# Pacote de visualização de dados
#
library(bnlearn)
library(StatMeasures)
library(plotly)

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
                  dados.grupos$grupoPrato,
                  dados.grupos$grupoAcompanhamento,
                  dados.grupos$grupoAdicional,
                  dados.grupos$grupoBrinde,
                  dados.grupos$venda)
  
  colnames(dados) <- c("mes", 
                       "quantidadeProduto", 
                       "grupoMilkShake", 
                       "grupoSanduiche",
                       "grupoBebida",
                       "grupoPrato",
                       "grupoAcompanhamento",
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
# Funcao - Ajuste Modelo - Rede Bayesiana Gaussiana - GS
#
fitModelBayesianNetworkGS <- function(training.set)
{
  bayesian.gs <- cextend(gs(training.set))
  
  plot(bayesian.gs)
  
  fit.bayesian.gs <- bn.fit(bayesian.gs, data = training.set, method="mle")
  
  #bn.fit.qqplot(fit.bayesian.gs,main = "Normal Q-Q Plot", xlab = )
  #bn.fit.histogram(fit.bayesian.gs)
  #bn.fit.xyplot(fit.bayesian.gs)
  return(fit.bayesian.gs)
}

#
# Funcao - Predição - Rede Bayesiana Gaussiana - GS
#
predictBayesianNetworkGS <- function(fit.bayesian.gs, test.set)
{
  predict.bayesian.gs <- predict(fit.bayesian.gs, "venda", test.set)
  return(predict.bayesian.gs)
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
plotBayesianNetworkGS <- function(ds.resultado)
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
              line = list(color = "rgb(156, 39, 176)", width = 3),
              name = "Modelo Rede Bayesiana Gaussiana - GS", 
              connectgaps = TRUE)
  p
}

# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Modelo Rede Bayesiana Gaussiana - GS
fit.bayesian.gs <- fitModelBayesianNetworkGS(training.set)

#Predição - Modelo Rede Bayesiana Gaussiana - GS
predict.bayesian.gs <- predictBayesianNetworkGS(fit.bayesian.gs, test.set)

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.bayesian.gs
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Médio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotBayesianNetworkGS(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]

dif <- previsto - real
sum(previsto)
sum(real)


previsto