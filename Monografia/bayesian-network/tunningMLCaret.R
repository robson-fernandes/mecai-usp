library(caret)

ls()
rm(list=ls())
graphics.off()


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

dados.grupos <<- read.table("dados-new.csv", 
                            header=TRUE, 
                            sep=";")
attach(dados.grupos)



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
training.setOriginal <<- dados[1:71, ]
training.set <- training.setOriginal
training.set[,"mes"] <- as.double(training.set[,"mes"])
training.set <- scale(training.set)
training.set <<- as.data.frame(training.set)


#
# Conjunto de Teste
#
test.setOriginal <<- dados[71:76, ]
test.set <- test.setOriginal
test.set[,"mes"] <- as.double(test.set[,"mes"])
test.set <- scale(test.set)
test.set <<- as.data.frame(test.set)


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

gbmFit1 <- train(venda ~ 
                   mes +
                   grupoBebidas +
                   grupoCarne  +
                   grupoUniforme +
                   grupoHortiFrutti +
                   grupoPaes + 
                   grupoSobremesas +
                   grupoMolhos +
                   grupoFrios, 
                 data = training.set, 
                 method = "elm", 
                 trControl = fitControl,
                 metric = "MAE")
gbmFit1
summary(gbmFit1)