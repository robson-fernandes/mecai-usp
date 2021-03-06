#Limpa workspace
ls()
rm(list=ls())
graphics.off()

#install.packages("plotly")
library(plotly)

indice <- c("1", "2", "3", "4", "5", "6")
real <- c("166619.4043", "138880.2904", "152376.5224", "164615.6087", "124614.5398", "97657.1001")
rbMMHC <- c("165793.6624", "146190.4542", "154297.8381", "147732.8826", "123982.7631", "106765.8654")
rbHC <- c("146532.5", "143441.3", "153714.1", "144524.5", "133913.9", "122637.1")
rbRSMAX2 <- c("141351.4578", "144561.6424", "147793.9304", "141119.5400", "137655.9156", "132280.9796")
rbGS <- c("141351.5", "144561.6", "147793.9", "141119.5", "137655.9", "132281.0")
rnBackProp <- c("161254.5844", "180900.4149", "175429.3491", "175043.8166", "126090.6017", "116522.7035")
bglm       <- c("161249.95", "138520.73", "168618.34", "153971.34", "123846.10", "98557.01")


trace1 <- list(
  x = indice, 
  y = real,
  connectgaps = FALSE, 
  line = list(shape = "spline"), 
  marker = list(opacity = 1), 
  mode = "lines+markers", 
  name = "Real", 
  showlegend = TRUE, 
  type = "scatter"
)
trace2 <- list(
  x = indice, 
  y = rbMMHC, 
  line = list(
    shape = "spline", 
    width = 3
  ), 
  marker = list(size = 6), 
  mode = "lines+markers", 
  name = "RB - MMHC", 
  type = "scatter"
)
trace3 <- list(
  x = indice, 
  y = rbHC, 
  line = list(shape = "spline"), 
  mode = "lines+markers", 
  name = "RB - HC", 
  type = "scatter"
)
trace4 <- list(
  x = indice, 
  y = rbGS, 
  line = list(shape = "spline"), 
  mode = "lines+markers", 
  name = "RB - GS", 
  type = "scatter"
)
trace5 <- list(
  x = indice, 
  y = rbRSMAX2, 
  line = list(shape = "spline"), 
  mode = "lines+markers", 
  name = "RB - RSMAX2", 
  type = "scatter"
)
trace6 <- list(
  x = indice, 
  y = rnBackProp, 
  line = list(shape = "spline"), 
  mode = "lines+markers", 
  name = "RN - Backpropagation Resiliente", 
  type = "scatter"
)

line.bglm <- list(
  x = indice, 
  y = bglm, 
  line = list(shape = "spline"), 
  mode = "lines+markers", 
  name = "BGLM - Bayesian Generalized Linear Models", 
  type = "scatter"
)

data <- list(trace1, trace2, trace3, trace4, trace5, trace6, line.bglm)
layout <- list(
  autosize = TRUE, 
  dragmode = "zoom", 
  hidesources = FALSE, 
  hovermode = "closest", 
  legend = list(
    x = 0.999182272294, 
    y = 0.676470588235
  ), 
  paper_bgcolor = "#fff", 
  plot_bgcolor = "rgb(255, 255, 255)", 
  separators = ".,", 
  showlegend = TRUE, 
  smith = FALSE, 
  title = "Modelos ajustados no hiperplano<br><b>Vendas Mensais</b>", 
  xaxis = list(
    anchor = "y", 
    autorange = TRUE, 
    color = "#444", 
    domain = c(0, 1), 
    dtick = "M12", 
    exponentformat = "e", 
    fixedrange = FALSE, 
    gridcolor = "rgb(238, 238, 238)", 
    gridwidth = 1, 
    hoverformat = "", 
    nticks = 0, 
    range = c(0.683620529924, 6.31637947008), 
    rangemode = "normal", 
    rangeselector = list(visible = FALSE), 
    showexponent = "all", 
    showgrid = TRUE, 
    showline = FALSE, 
    showticklabels = TRUE, 
    side = "bottom", 
    tick0 = 946702800000, 
    tickangle = "auto", 
    tickfont = list(
      color = "#444", 
      family = "\"Open Sans\", verdana, arial, sans-serif", 
      size = 12
    ), 
    tickformat = "", 
    tickmode = "auto", 
    tickprefix = "", 
    ticks = "", 
    ticksuffix = "", 
    title = "Indice", 
    titlefont = list(
      color = "#444", 
      family = "\"Open Sans\", verdana, arial, sans-serif", 
      size = 14
    ), 
    type = "linear", 
    zeroline = TRUE, 
    zerolinecolor = "#444", 
    zerolinewidth = 1
  ), 
  yaxis = list(
    anchor = "x", 
    autorange = TRUE, 
    color = "#444", 
    domain = c(0, 1), 
    dtick = 50, 
    exponentformat = "B", 
    fixedrange = FALSE, 
    gridcolor = "rgb(238, 238, 238)", 
    gridwidth = 1, 
    hoverformat = "", 
    nticks = 0, 
    range = c(6822.16131935, 20971.0815807), 
    rangemode = "normal", 
    showexponent = "all", 
    showgrid = TRUE, 
    showline = FALSE, 
    showticklabels = TRUE, 
    side = "left", 
    tick0 = 0, 
    tickangle = "auto", 
    tickfont = list(
      color = "#444", 
      family = "\"Open Sans\", verdana, arial, sans-serif", 
      size = 12
    ), 
    tickformat = "", 
    tickmode = "auto", 
    tickprefix = "", 
    ticks = "", 
    ticksuffix = "", 
    title = "Venda (R$)", 
    titlefont = list(
      color = "#444", 
      family = "\"Open Sans\", verdana, arial, sans-serif", 
      size = 14
    ), 
    type = "linear", 
    zeroline = TRUE, 
    zerolinecolor = "#444", 
    zerolinewidth = 1
  )
)
p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, connectgaps=trace1$connectgaps, line=trace1$line, marker=trace1$marker, mode=trace1$mode, name=trace1$name, showlegend=trace1$showlegend, type=trace1$type, uid=trace1$uid, xsrc=trace1$xsrc, ysrc=trace1$ysrc)
p <- add_trace(p, x=trace2$x, y=trace2$y, line=trace2$line, marker=trace2$marker, mode=trace2$mode, name=trace2$name, type=trace2$type, uid=trace2$uid, xsrc=trace2$xsrc, ysrc=trace2$ysrc)
p <- add_trace(p, x=trace3$x, y=trace3$y, line=trace3$line, mode=trace3$mode, name=trace3$name, type=trace3$type, uid=trace3$uid, xsrc=trace3$xsrc, ysrc=trace3$ysrc)
p <- add_trace(p, x=trace4$x, y=trace4$y, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type, uid=trace4$uid, xsrc=trace4$xsrc, ysrc=trace4$ysrc)
p <- add_trace(p, x=trace5$x, y=trace5$y, line=trace5$line, mode=trace5$mode, name=trace5$name, type=trace5$type, uid=trace5$uid, xsrc=trace5$xsrc, ysrc=trace5$ysrc)
p <- add_trace(p, x=line.bglm$x, y=line.bglm$y, line=line.bglm$line, mode=line.bglm$mode, name=line.bglm$name, type=line.bglm$type, uid=trace6$uid, xsrc=line.bglm$xsrc, ysrc=line.bglm$ysrc)
p <- add_trace(p, x=trace6$x, y=trace6$y, line=trace6$line, mode=trace6$mode, name=trace6$name, type=trace6$type, uid=trace6$uid, xsrc=trace6$xsrc, ysrc=trace6$ysrc)
p <- layout(p, autosize=layout$autosize, dragmode=layout$dragmode, hidesources=layout$hidesources, hovermode=layout$hovermode, legend=layout$legend, paper_bgcolor=layout$paper_bgcolor, plot_bgcolor=layout$plot_bgcolor, separators=layout$separators, showlegend=layout$showlegend, smith=layout$smith, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
p