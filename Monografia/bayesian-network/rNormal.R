
m = read.table('movimentacao.csv', header=TRUE, sep=";")
movimentacao = m[1:100000,]

nRows <- nrow(movimentacao)

for(i in 1:nRows){
  
   if (movimentacao[i,"quantidade"] > 0 & movimentacao[i,"quantidade"] <=10) movimentacao[i,"quantidadeCategorico"] <- "0-10"
   else if (movimentacao[i,"quantidade"] > 10 & movimentacao[i,"quantidade"] <=20) movimentacao[i,"quantidadeCategorico"] <- "10-20"
   else if (movimentacao[i,"quantidade"] > 20 & movimentacao[i,"quantidade"] <=30) movimentacao[i,"quantidadeCategorico"] <- "20-30"
   else if (movimentacao[i,"quantidade"] > 30 & movimentacao[i,"quantidade"] <=40) movimentacao[i,"quantidadeCategorico"] <- "30-40"
   else if (movimentacao[i,"quantidade"] > 40 & movimentacao[i,"quantidade"] <=50) movimentacao[i,"quantidadeCategorico"] <- "40-50"
   else if (movimentacao[i,"quantidade"] > 50 & movimentacao[i,"quantidade"] <=60) movimentacao[i,"quantidadeCategorico"] <- "50-60"
   else if (movimentacao[i,"quantidade"] > 60 & movimentacao[i,"quantidade"] <=70) movimentacao[i,"quantidadeCategorico"] <- "60-70"
   else
     movimentacao[i,"quantidadeCategorico"] <- "outro"
}

attach(movimentacao)

library(bnlearn)


bnMovimentacao <- data.frame(movimentacao[1:100000,c("quantidadeCategorico","diaSemana", "codTipoVenda")])

bnMovimentacao$quantidadeCategorico <- as.factor(bnMovimentacao$quantidadeCategorico)
bnMovimentacao$diaSemana            <- as.factor(bnMovimentacao$diaSemana)
bnMovimentacao$codTipoVenda         <- as.factor(bnMovimentacao$codTipoVenda)
#Criacao da Rede
res <- hc(bnMovimentacao)
res
plot(res)

