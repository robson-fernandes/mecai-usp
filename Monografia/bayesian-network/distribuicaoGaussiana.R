# Suponha que a quantidade de vendas de um produto se enquadra em uma distribuição normal.
# Além disso, o escore médio das vendas é 29 e o desvio padrão é 24,59. 

# Qual é a porcentagem de vendas acima de 30 ou mais?


quantidadeVenda <- c(60,20, 30, 35, 10, 10, 50, 5)
media = mean(quantidade)
desvioPadrao = sd(quantidade);

valor = 30

pNormal = pnorm(valor, mean=media, sd=desvioPadrao, lower.tail = FALSE) 
pNormal

#48% de Vendas