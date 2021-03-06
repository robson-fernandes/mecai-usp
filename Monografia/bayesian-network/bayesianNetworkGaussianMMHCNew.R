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


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
 
  NA2median <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  
  NA2median(y)
}



#
# Funcao - Carregar conjunto de Treinamento e Teste
#
loadDataSet <- function()
{
  dados.grupos <<- read.table("dados-new.csv", 
                              header=TRUE, 
                              sep=";")
  attach(dados.grupos)

  
  #dados.grupos$grupoCarne <- remove_outliers(dados.grupos$grupoCarne)
  #dados.grupos$grupoUniforme <- remove_outliers(dados.grupos$grupoUniforme)
  #dados.grupos$grupoEmbalagem <- remove_outliers(dados.grupos$grupoEmbalagem)
  #dados.grupos$grupoUsoConsumo <- remove_outliers(dados.grupos$grupoUsoConsumo)
  #dados.grupos$grupoHortiFrutti <- remove_outliers(dados.grupos$grupoHortiFrutti)
  #dados.grupos$grupoBebidas <- remove_outliers(dados.grupos$grupoBebidas)
  #dados.grupos$grupoSobremesas <- remove_outliers(dados.grupos$grupoSobremesas)
  #dados.grupos$grupoMolhos <- remove_outliers(dados.grupos$grupoMolhos)
  #dados.grupos$grupoFrios <- remove_outliers(dados.grupos$grupoFrios)
  #dados.grupos$grupoSecos <- remove_outliers(dados.grupos$grupoSecos)
  
  dados <-  cbind(dados.grupos$mes, 
                  dados.grupos$venda, 
                  dados.grupos$grupoCarne,
                  dados.grupos$grupoUniforme, 
                  dados.grupos$grupoEmbalagem, 
                  dados.grupos$grupoUsoConsumo,
                  dados.grupos$grupoHortiFrutti,
                  dados.grupos$grupoPaes,
                  dados.grupos$grupoBebidas,
                  dados.grupos$grupoSobremesas,
                  dados.grupos$grupoMolhos,
                  dados.grupos$grupoFrios,
                  dados.grupos$grupoSecos
  )
  
  
  colnames(dados) <- c("mes", 
                       "venda", 
                       "grupoCarne",
                       "grupoUniforme", 
                       "grupoEmbalagem",
                       "grupoUsoConsumo",
                       "grupoHortiFrutti",
                       "grupoPaes",
                       "grupoBebidas",
                       "grupoSobremesas",
                       "grupoMolhos",
                       "grupoFrios",
                       "grupoSecos"
  )
  
  
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
  bayesian.mmhc <- bnlearn::hc(training.set)
  bayesian.mmhc <<- bayesian.mmhc
  
  plot(bayesian.mmhc)
  
  fit.bayesian.mmhc <- bn.fit(bayesian.mmhc, data = training.set, method="mle")
  
  return(fit.bayesian.mmhc)
}

#
# Funcao - Predição - Rede Bayesiana Gaussiana - MMHC
#
predictBayesianNetworkMMHC <- function(fit.bayesian.mmhc, test.set)
{
  predict.bayesian.mmhc <- predict(fit.bayesian.mmhc, "venda", test.set)
  return(predict.bayesian.mmhc)
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
               line = list(shape = "spline"),
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

#Predição - Modelo Rede Bayesiana Gaussiana - MMHC
predict.bayesian.mmhc <- predictBayesianNetworkMMHC(fit.bayesian.mmhc, test.set[1:9])

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.bayesian.mmhc
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)


#Erro Percentual Absoluto Médio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotBGLM(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]

dif <- previsto - real
sum(previsto)
sum(real)
