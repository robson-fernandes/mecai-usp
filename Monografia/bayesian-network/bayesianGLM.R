# ------------------------------------------
# Modelo Linear Generalizado Bayesiano
# ------------------------------------------

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()

#
# Pacotes - 
#
# <arm>
# Pacote de analise de dados e modelos hierarquicos de regressao
#
# <StatMeasures>
# Pacote de verificacoes estatisticas 
#
# <plotly>
# Pacote de visualizacao de dados
#
library(arm) 
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
                  dados.grupos$venda, 
                  dados.grupos$grupoMilkShake, 
                  dados.grupos$grupoSanduiche, 
                  dados.grupos$grupoBebida,
                  dados.grupos$grupoAcompanhamento,
                  dados.grupos$grupoPrato,
                  dados.grupos$grupoAdicional,
                  dados.grupos$grupoBrinde,
                  dados.grupos$grupoItensComposicao)
  
  colnames(dados) <- c("mes", 
                       "quantidadeProduto", 
                       "venda",
                       "grupoMilkShake", 
                       "grupoSanduiche",
                       "grupoBebida",
                       "grupoAcompanhamento",
                       "grupoPrato",
                       "grupoAdicional",
                       "grupoBrinde",
                       "grupoItensComposicao")
  
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
# Funcao - Ajuste Modelo - Modelo Linear Generalizado Bayesiano
#
fitModelBayesGLM <- function(training.set)
{
    fit.bayesian.glm <- bayesglm(venda ~ 
                          mes +
                          quantidadeProduto  +
                          grupoMilkShake + 
                          grupoSanduiche + 
                          grupoBebida +
                          grupoAcompanhamento +
                          grupoPrato + 
                          grupoAdicional,
                          family=gaussian(link = "identity"),
                          data= training.set,
                          prior.df= Inf,
                          prior.mean = 0,
                          prior.scale = NULL,
                          maxit = 100)
    
    summary(fit.bayesian.glm)
    return(fit.bayesian.glm)
}

#
# Funcao - Predicao - Modelo Linear Generalizado Bayesiano
#
predictBayesGLM <- function(fit.bayesian.glm, test.set)
{
  predict.bayesian.glm <- predict.glm(fit.bayesian.glm,
                                      newdata = as.data.frame(test.set),
                                      se.fit = T)
  return(predict.bayesian.glm)
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
              line = list(color = 'rgb(255, 87, 34)', width = 3),
              name = "Modelo Linear Generalizado Bayesiano", 
              connectgaps = TRUE)
  p
}


# Carrega conjunto de treinamento e teste
loadDataSet()

#Ajuste Modelo - Modelo Linear Generalizado Bayesiano
fit.bayesian.glm <- fitModelBayesGLM(training.set)

#Predicao - Modelo Linear Generalizado Bayesiano
predict.bayesian.glm <- predictBayesGLM(fit.bayesian.glm, test.set)

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.bayesian.glm$fit
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Medio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotBGLM(ds.resultado)
