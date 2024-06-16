library(highcharter)
library(dplyr)
library(tidyr)

file = "dados_2019-2021.csv"

df = read.csv(file, header = TRUE, sep = ";")

#contagem WHAT
d1 <- df%>%
  dplyr::group_by(WHAT)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(WHATNew = paste(WHAT, n, '(', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)
d1

dMain <- merge (df, d1, by  = 'WHAT')

#contagem WHO
d2 <- df%>%
  dplyr::group_by(WHO)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(WHONew = paste(WHO, n, '(', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

dMain <- merge (dMain, d2, by  = 'WHO')

#contagem dos tipos
d3 <- df%>%
  dplyr::group_by(HOW)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(HOWNew = paste(HOW, n, '(', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

dMain <- merge (dMain, d3, by  = 'HOW')

dFinal <- dMain%>%
  dplyr::select(WHATNew, HOWNew,WHONew)

my_theme <- hc_theme(
chart = list(
  
  style = list(
    fontFamily = "Montserrat",
    color ='black'
    
  )
))


hchart(data_to_sankey(dFinal), "sankey", name = "")%>% 
  hc_add_theme(my_theme)
