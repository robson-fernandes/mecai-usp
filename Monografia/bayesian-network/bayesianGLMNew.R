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
  dados.grupos <- read.table("dados-new.csv", 
                             header=TRUE, 
                             sep=";")
  attach(dados.grupos)
  
  dados.grupos <<- outlierKD(dados.grupos, grupoCarne)
  dados.grupos <<- outlierKD(dados.grupos, grupoUniforme)
  dados.grupos <<- outlierKD(dados.grupos, grupoEmbalagem)
  dados.grupos <<- outlierKD(dados.grupos, grupoUsoConsumo)
  dados.grupos <<- outlierKD(dados.grupos, grupoHortiFrutti)
  dados.grupos <<- outlierKD(dados.grupos, grupoPaes)
  dados.grupos <<- outlierKD(dados.grupos, grupoBebidas)
  dados.grupos <<- outlierKD(dados.grupos, grupoSobremesas)
  dados.grupos <<- outlierKD(dados.grupos, grupoMolhos)
  dados.grupos <<- outlierKD(dados.grupos, grupoFrios)
  dados.grupos <<- outlierKD(dados.grupos, grupoSecos)
  
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
# Funcao - Ajuste Modelo - Modelo Linear Generalizado Bayesiano
#
fitModelBayesGLM <- function(training.set)
{

  fit.bayesian.glm <- glm(venda ~ 
                            mes +
                            grupoBebidas +
                            grupoCarne  +
                            grupoUniforme +
                            grupoHortiFrutti +
                            grupoPaes + 
                            grupoSobremesas +
                            grupoMolhos +
                            grupoFrios,
                               family=gaussian(link = "identity"),
                               data= training.set)
  
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
               line = list(shape = "spline"),
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

#Análise de Resíduos
resid(fit.bayesian.glm) #List of residuals
hist(resid(fit.bayesian.glm))
plot(density(resid(fit.bayesian.glm))) #A density plot
qqnorm(resid(fit.bayesian.glm)) # A quantile normal plot - good for checking normality
qqline(resid(fit.bayesian.glm))

#Gerar Conjunto de Dados - Real vs Previsto
real <- test.setOriginal[,"venda"]
previsto <- predict.bayesian.glm$fit
ds.resultado <- getDataSet.RealvsPrevisto(real, previsto)

#Erro Percentual Absoluto Medio
getMape(ds.resultado)

# Visualizar Gráfico do Modelo
plotBGLM(ds.resultado)


previsto <- ds.resultado[,"previsto"]
real <- ds.resultado[,"real"]

dif <- previsto - real
sum(previsto)
sum(real)