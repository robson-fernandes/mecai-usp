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

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- "y" #readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(dt)
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

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