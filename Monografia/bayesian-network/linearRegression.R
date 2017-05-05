movimentacao = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(movimentacao)

modelo = lm(quantidade ~ vlrUnitario + codGrupoVenda + codTipoVenda + mes, family=binomial(link="logit"));
modelo
summary(modelo)
