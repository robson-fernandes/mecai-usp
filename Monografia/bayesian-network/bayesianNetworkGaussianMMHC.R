# ------------------------------------------
# Rede Bayesiana Gaussiana - MMHC
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

#
# Pacotes - 
#
# <bnlearn>
# Pacote Análise e Inferência em Redes Bayesianas
#
# <StatMeasures>
# Pacote de verificacoes estatisticas
#
# <plotly>
# Pacote de visualizacao de dados
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
# Funcao - Ajuste Modelo - Rede Bayesiana Gaussiana - MMHC
#
fitModelBayesianNetworkMMHC <- function(training.set)
{
  bayesian.mmhc <- mmhc(training.set)
  
  bayesian.mmhc <- set.arc(bayesian.mmhc, "mes", "quantidadeProduto")
  bayesian.mmhc <- set.arc(bayesian.mmhc, "quantidadeProduto", "venda")
  bayesian.mmhc <- set.arc(bayesian.mmhc, "grupoSanduiche", "venda")
  bayesian.mmhc <- set.arc(bayesian.mmhc, "grupoMilkShake", "venda")
  bayesian.mmhc <- set.arc(bayesian.mmhc, "grupoAcompanhamento", "venda")
  
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "venda", "mes")
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "grupoBrinde", "venda")
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "quantidadeProduto", "grupoAcompanhamento")
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "quantidadeProduto", "grupoMilkShake")
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "quantidadeProduto", "grupoSanduiche")
  bayesian.mmhc <- drop.arc(bayesian.mmhc, "grupoSanduiche", "grupoMilkShake")
  
  plot(bayesian.mmhc)
  
  fit.bayesian.mmhc <- bn.fit(bayesian.mmhc, data = training.set, method="mle")
  return(fit.bayesian.mmhc)
}

#
# Funcao - Predicao - Rede Bayesiana Gaussiana - MMHC
#
predictBayesianNetworkMMHC <- function(fit.bayesian.mmhc, test.set)
{
  predict.bayesian.mmhc <- predict(fit.bayesian.mmhc, "venda", test.set)
  return(predict.bayesian.mmhc)
}

#
# Funcao - Conversao de Valores normalizados em escala para Original
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
# Funcao - Erro Percentual Absoluto Medio
#
getMape <- function(data.set)
{
  mape <- mape(y = data.set[, "real"], 
               yhat = data.set[, "previsto"])
  return(mape*100)
}

#
# Funcao - Visualizar grafico do modelo
#
plotBGLM <- function(ds.resultado)
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
              name = "Modelo Rede Bayesiana Gaussiana - MMHC", 
              connectgaps = TRUE)
  p
}

# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Modelo Rede Bayesiana Gaussiana - MMHC
fit.bayesian.mmhc <- fitModelBayesianNetworkMMHC(training.set)

#Predicao - Modelo Rede Bayesiana Gaussiana - MMHC
predict.bayesian.mmhc <- predictBayesianNetworkMMHC(fit.bayesian.mmhc, test.set)

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.bayesian.mmhc
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Medio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotBGLM(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]
sum(previsto)
sum(real)