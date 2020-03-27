# INF-0612 Análise de dados                                    #
#                                                              #
# Projeto final                                                #
#--------------------------------------------------------------#
# Nome COMPLETO Aluna (o) 1: Guilherme Ramos                   #
#                             Gouveia                          #
#                                                              #
# Nome COMPLETO Aluna (o) 2: Marina Abichabki                  #
#                                     Pivato                   #
#                                                              #
# Nome COMPLETO Aluna (o) 3: Paola São Thiago                  #
#                                                              #
#                                                              #
# Nome COMPLETO Aluna (o) 4:                                   #
#                                                              #
#                                                              #
#--------------------------------------------------------------#

#--------------------------------------------------------------#
#     Configuracao dos arquivos, libs e funçÃµes auxiliares     #
#--------------------------------------------------------------#
setwd('/home/grgouveia/studies/mdc/INF-0612-I/trabalho-final')
pwd <- getwd()

install.packages('tidyverse')
install.packages("DT")
library(tidyverse)
library(ggplot2)
library(dplyr)

# extrai hora de um Date em POSIXct
getHour <- function(time) {
  time <- as.POSIXlt(time);
  unclass(time)$hour
}

# retorna periodo do dia de acordo com a hora
getPeriod <- function(hour){
  if (hour > 0 & hour < 6) return("madrugada")
  else if (hour >= 6 & hour < 12) return("manhã")
  else if (hour >= 12 & hour < 18) return("tarde")
  else return("noite")
}


# Filtra o dataframe (df) passado como argumento
# de acordo com um intervalo (interval) em ano
# ou mÃªs, sendo estes critérios definidos pela
# variÃ¡vel attr. Os Valores possÃ?veis para
# os atributos são mes e ano.
getYears <- function(col) {
  unclass(col)$year + 1900
}

getMonths <- function(col) {
  unclass(col)$mon + 1
}

getDay <- function(col) {
  day <- as.POSIXlt(col)
  day <- unclass(day)$mday
}

addDateColumns <- function(df) {
  date <- as.POSIXlt(df$horario)
  df$ano <- unclass(date)$year + 1900
  df$mes <- unclass(date)$mon + 1
  df$dia <- unclass(date)$mday
  return(df)
}

filterBy <- function(attr, df, interval) {
  date <- as.POSIXlt(df$horario)
  ifelse(attr == "ano", df$ano <- getYears(date), df$mes <- getMonths(date))
  df<-df[df$attr %in% interval,]
  print(df)
}

# checa se o valor da linha é NA
is_na <- function(row){
  any(is.na(row))
}

# analisa se os dados nas k posiçÃµes Ã  frente e atrÃ s da
# posição especificada são repetidos para encontrar
# valores consecutivos repetidos
consecutive <- function(vector, k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n)
    if (all(vector[(i-k):(i-1)] == vector[i]))
      result[i] <- TRUE
  for (i in 1:(n-k))
    if (all(vector[(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
 return(result)
}
#--------------------------------------------------------------#
#     1. Tratamento de  dados                                  #
#--------------------------------------------------------------#
#     1.1 Carregando dados                                     #
#--------------------------------------------------------------#
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv", "r")
cepagri <- read.csv(con, header = FALSE,
                    sep = ";",
                    fill = TRUE,
                    col.names = names)

head(cepagri)
close(con)

#Observacao dos dados
summary(cepagri)

#--------------------------------------------------------------#
#     1. Processando dados                                     #
#--------------------------------------------------------------#
#     1.2 Removendo linhas com valor NA                        #
#--------------------------------------------------------------#
na_percent <- paste(sum(is.na(cepagri))/nrow(cepagri)*100,"%")
if (na_percent > 0) {
  is_na <- apply(cepagri, 1, is_na)
  cepagri <- cepagri[!is_na,];cepagri
  confirm_removal <- !any(is.na(cepagri)); paste("Rows with NA data", ifelse(confirm_removal, "removed.", "removal failed."))
}

#--------------------------------------------------------------#
#     1. Processando dados                                     #
#--------------------------------------------------------------#
#     1.3 Corrigindo coerçÃµes implÃ?citas indesejadas           #
#--------------------------------------------------------------#
for (i in 2:length(cepagri)) {
  aux <- cepagri[,i]
  if(is.factor(aux)) {
    aux <- as.character(aux)
    aux <- as.numeric(aux)
  }
  print(class(aux))
  cepagri[,i] <- aux
}

#--------------------------------------------------------------#
#     1. Processando dados                                     #
#--------------------------------------------------------------#
#     1.4 Removendo outliers                                   #
#--------------------------------------------------------------#
## Analisando discrepancia de cada informacao e
## removendo outliers
#sensa
summary(cepagri$sensa)
cepagri[cepagri$sensa == 99.9, 5] <- NA

#umid
summary(cepagri$umid)
cepagri[cepagri$umid == 0,]
sort(cepagri[cepagri$umid < 5,4])
umid_muito_baixa<-cepagri[cepagri$umid < 5,4]
#ocorrem muito nas medicoes entre 07:00 e 07:10 e apenas esse sensor com 0, parece um erro do sensor
#alem disso nao existe nenhum outro valor perto de 0, nem menor que 5
#Para o valor maior
umid_muito_alta<-sort(cepagri[cepagri$umid > 95 & cepagri$umid!=100 ,4 ], decreasing = TRUE)
#ocorrem valores proximos de 100, entao 100 parece um valor valido

cepagri[cepagri$umid == 0, 4] <- NA

#--------------------------------------------------------------#
#     1. Processando dados                                     #
#--------------------------------------------------------------#
#     1.5 Convertendo coluna de data p/ POSIXct
#--------------------------------------------------------------#
cepagri$horario <- as.POSIXct(
                as.character(cepagri$horario),
                format='%d/%m/%Y-%H:%M')


#--------------------------------------------------------------#
#     1. Processando dados                                     #
#--------------------------------------------------------------#
#     1.6 Análise de registros duplicados                      #
#         Valores repetidos durante dias consectivos           #
#--------------------------------------------------------------#
# filtra os valores recorrentes em 144 dias consecutivos
filtro <- consecutive(cepagri$temp, 144)
length(unique(as.Date(cepagri[filtro, 1])))

#umid
summary(cepagri$vento)
sort(cepagri[cepagri$vento < 5,3])
#ocorrem valores proximos de 0, entao 0 parece um valor valido
#sobre o valor mais alto, 147, pesquisando na internet foi uma medicao verifica
#-----------------------------------------------#
#        Análise registros duplicados           #
#-----------------------------------------------#

#install.packages('tidyverse')
library(tidyverse)

#Retorna as linhas duplicadas do data frame
cepagri[duplicated(cepagri),]

# Exemplo de filtragem de uma linha duplidada
cepagri[cepagri$horario == '2015-01-23 09:24:00',]

# Retorna as linhas duplicadas do data frame
cepagri[duplicated(cepagri),]
# Remove linhas duplicadas
cepagri <- cepagri[!duplicated(cepagri),]

#verifica se ainda tem linhas duplicadas
cepagri[duplicated(cepagri),]
#--------------------------------------------------------------#
#     2. Analisando dados                                      #
#--------------------------------------------------------------#
#     2.1 Agrupando dados por mês e ano                        #
#      Filtro de dados agrupados com base em intervalos        #
#--------------------------------------------------------------#
intervalo <- list(2015, 2016, 2017, 2018, 2019)
filterByYear <- filterBy("ano", cepagri, intervalo)
filterByMonth <- filterBy("mes", cepagri, intervalo)

# adiciona colunas de dia, mes e ano
cepagri <- addDateColumns(cepagri)

# Temperatura média de cada ano
temp_media_ano <- tapply(cepagri$temp, cepagri$ano, mean)

# Temperatura média de cada mÃªs
temp_media <- tapply(cepagri$temp , cepagri$mes , mean)
temp_media <- round(temp_media)
temp_media

#--------------------------------------------------------------#
#     2. Analisando dados                                      #
#--------------------------------------------------------------#
#                                                              #
#     2.2 Medidas de posição com a base tratada                #
#--------------------------------------------------------------#
summary(cepagri)

#--------------------------------------------------------------#
#     2. Analisando dados                                      #
#--------------------------------------------------------------#
#                                                              #
#     2.3 Boxplot                                              #
#--------------------------------------------------------------#
cepagriDataByYear <- list()
i <- 0
for (ano in unique(cepagri$ano)) {
  cepagriDataByYear[[i <- i+1]] <- cepagri[cepagri$ano == ano, ]
  cepagriDataByYear[[i]]$mes <- as.factor(cepagriDataByYear[[i]]$mes)

  title <- paste("Temperatura no ano de", ano)
  plot <- ggplot(cepagriDataByYear[[i]],
            aes(x = mes , y = temp , group = mes)) +
            geom_boxplot () +
            theme_light() +
            labs(colour = element_blank(),
                title = title) +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position = c(0.5, 0.15)) +
            theme(legend.box.background = element_rect(colour = "black"))
  print(plot)
}

#--------------------------------------------------------------#
#     2. Analisando dados                                      #
#--------------------------------------------------------------#
#                                                              #
#     2.3 Análise de dados por períodos                        #
#                                                              #
# Primavera: 21 setembro até 20 dezembro                       #         
# Verão: 21 dezembro até 20 março                              #
# Outono: 21 março até 20 junho                                #
# Inverno: 21 junho até 20 setembro                            #
##-------------------------------------------------------------#
# classifica cada medição de acordo com o período do dia
# (madrugada, manhã, tarde, noite)
periodos <- c("madrugada", "manhã", "tarde", "noite")
cepagri$hora <- getHour(cepagri$horario)
cepagri$periodo <- lapply(cepagri$hora, getPeriod)

#######################################
# TODO
# ISSO AQUI AINDA NÃO TÁ FUNCIONANDO
########################################
# filtra e armazena em listas as medições de acordo com os períodos
cepagri_periodos <- list()
for (p in periodos) {
  aux <- cepagri[cepagri$periodo == p, ]
  if (any(aux == p)) {print("SIIIIIIIIIIIIIIIIIIIIM")}
  cepagri_periodos <- list(cepagri_periodos, aux)
}

#----------------Medidas de Dispersão
# Desvio padrão
dp <- c()
media <- c()
coef_var <- c()
for(i in 2:5){
    #Calculo desvio padrão para colunas 2:5
    dp <- round(c(dp,sd(cepagri[,i],na.rm = TRUE)),2)
    #calculo média colunas 2:5
    media <-round(c(media, mean(cepagri[,i],na.rm = TRUE)))

}
#Coeficiente de variação
coef_var <- round(c(coef_var, (dp/media)*100),2)
# Tabela que mostra a média, desvio padrão e coeficiente de variação de cada coluna
variaveis <- c('temp','vento','umid','sensa')
medidas_dispersao <-data.frame(variaveis,media,dp,coef_var); medidas_dispersao


#-------------------------Histogramas
# Histograma de cada coluna
hist(cepagri$temp, row = 'green', main = 'Histograma Temperatura', xlab = 'Temperatura', ylab = 'FrequÃªncia')
hist(cepagri$sensa, row = 'red', main = 'Histograma Sensação Térmica', xlab = 'sensação térmica', ylab = 'FrequÃªncia')
hist(cepagri$vento, row = 'gray', main = 'Histograma Vento', xlab = 'Vento', ylab = 'FrequÃªncia')
hist(cepagri$umid, row = 'blue', main = 'Histograma Umidade', xlab = 'Umidade', ylab = 'FrequÃªncia')


#-------------------------Analisando relacao temperatura umidade e sensacao termica
cepagri_analise <- cepagri

#--------------------------------------------------------------#
#     3. Analise de dados                                      #
#--------------------------------------------------------------#
#     3.2 Comparativo entre as medidas                         #
#--------------------------------------------------------------#
cepagri_analise <- cepagri
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$temp), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$vento), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$sensa), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$umid), ]
cepagri_analise$diferenca_temp_sensa <- cepagri_analise$temp-cepagri_analise$sensa

dados_medios<-group_by(cepagri_analise, mes)%>%summarise(Temperatura=mean(temp), Umidade=mean(umid), Sensacao=mean(sensa), Vento=mean(vento), Diferenca_temp_sensa=mean(diferenca_temp_sensa))


dados_medios_normalizados<-dados_medios

dados_medios_normalizados$Temperatura<-(dados_medios$Temperatura-min(dados_medios$Temperatura))/(max(dados_medios$Temperatura)-min(dados_medios$Temperatura))
dados_medios_normalizados$Umidade<-(dados_medios$Umidade-min(dados_medios$Umidade))/(max(dados_medios$Umidade)-min(dados_medios$Umidade))
dados_medios_normalizados$Vento<-(dados_medios$Vento-min(dados_medios$Vento))/(max(dados_medios$Vento)-min(dados_medios$Vento))
dados_medios_normalizados$Sensacao<-(dados_medios$Sensacao-min(dados_medios$Sensacao))/(max(dados_medios$Sensacao)-min(dados_medios$Sensacao))
dados_medios_normalizados$Diferenca_temp_sensa<-(dados_medios$Diferenca_temp_sensa-min(dados_medios$Diferenca_temp_sensa))/(max(dados_medios$Diferenca_temp_sensa)-min(dados_medios$Diferenca_temp_sensa))

dados_tabela_para_relatorio<-round(dados_medios, digits = 2)

DT::datatable(round(dados_medios, digits = 2), fillContainer = TRUE, rownames = FALSE, caption = "Valores médios por mês", autoHideNavigation=TRUE)



ggplot(dados_medios, aes(x = mes)) +
  geom_point(aes(y = Temperatura, colour = "Temperatura media")) +
  geom_line(aes(y = Temperatura, colour = "Temperatura media")) +
  
  geom_point(aes(y = Umidade, colour = "Umidade media")) +
  geom_line(aes(y = Umidade, colour = "Umidade media")) +
  
  geom_point(aes(y = Sensacao, colour = "Sensacao media")) +
  geom_line(aes(y = Sensacao, colour = "Sensacao media")) +
  
  geom_point(aes(y = Vento, colour = "Velocidade Vento media")) +
  geom_line(aes(y = Vento, colour = "Velocidade Vento media")) +

  
  theme_light() +
  labs(colour = element_blank(),
       title = "Visualizacao dos dados da tabela sem normalizar") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.15, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "mes", limits = c(1, 12),
                     breaks =  0:12,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "", limits = c(0, 100),
                     minor_breaks = NULL)


ggplot(dados_medios_normalizados, aes(x = mes)) +
  geom_point(aes(y = Temperatura, colour = "Temperatura media")) +
  geom_line(aes(y = Temperatura, colour = "Temperatura media")) +
  
  geom_point(aes(y = Umidade, colour = "Umidade media")) +
  geom_line(aes(y = Umidade, colour = "Umidade media")) +
  
  geom_point(aes(y = Sensacao, colour = "Sensacao media")) +
  geom_line(aes(y = Sensacao, colour = "Sensacao media")) +
  
  geom_point(aes(y = Vento, colour = "Velocidade Vento media")) +
  geom_line(aes(y = Vento, colour = "Velocidade Vento media")) +
  
  theme_light() +
  labs(colour = element_blank(),
       title = "Comparativo dos valores medios normalizados ao mes de todo periodo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.1, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "mes", limits = c(1, 12),
                     breaks =  0:12,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "", limits = c(0, 1.3),
                     minor_breaks = NULL)







#-------------------------Analisando estacoes verao e inverno
#-------------------------------------------------------------------------------------------------------------------------#
# Obs: da forma como foi feito, está sendo considerado como verão de 2015, por exemplo, os meses 01,02,03 e 12 de 2015.   #
# Mas, o mês 12 de 2015 faz parte do verão de 2016                                                                        #
#-------------------------------------------------------------------------------------------------------------------------#
cepagri_analise <- cepagri
cepagri_analise$dia<-getDay(cepagri_analise$horario)

cepagri_verao<-cepagri_analise[((cepagri_analise$mes==12&cepagri_analise$dia>=21) | (cepagri_analise$mes==1) | (cepagri_analise$mes==2) |  (cepagri_analise$mes==3 & cepagri_analise$dia<=20)),]
cepagri_inverno<-cepagri_analise[((cepagri_analise$mes==6&cepagri_analise$dia>=21) | (cepagri_analise$mes==7) | (cepagri_analise$mes==8) |  (cepagri_analise$mes==9 & cepagri_analise$dia<=20)),]
#-----------------------------------------------------------------------------------------#
#Gráficos de linhas para avaliar o comportamento da temperatura e sensação termica por dia durante o verão e inverno
#-----------------------------------------------------------------------------------------#
# média da temperatura e sensação termica da dia durante um determinada ano no período do #verão
#média da temperatura e sensação termica da dia durante um determinada ano no período do inverno

# Gera outro dataframe, transforma as colunas ano, mês e dia para o formato Date e agrupa em uma coluna
cepagri_verao2 <- cepagri_verao
cepagri_verao2$data <- as.Date(paste(cepagri_verao2$ano, cepagri_verao2$mes, cepagri_verao2$dia, sep = "-"))

#Seta para todos os anos o intervalo de tempo do verão
dados_verao_2015 <- cepagri_verao2[(cepagri_verao2$data >= '2014-12-21' & cepagri_verao2$data <= '2015-3-21'),]
dados_verao_2016 <- cepagri_verao2[(cepagri_verao2$data >= '2015-12-21' & cepagri_verao2$data <= '2016-3-21'),]
dados_verao_2017 <- cepagri_verao2[(cepagri_verao2$data >= '2016-12-21' & cepagri_verao2$data <= '2017-3-21'),]
dados_verao_2018 <- cepagri_verao2[(cepagri_verao2$data >= '2017-12-21' & cepagri_verao2$data <= '2018-3-21'),]
dados_verao_2019 <- cepagri_verao2[(cepagri_verao2$data >= '2018-12-21' & cepagri_verao2$data <= '2019-3-21'),]

media_verao_temp_sensa <- group_by(dados_ver_inv_2017, data)%>%summarise(TempMedia=mean(temp), SensaMedia=mean(sensa))

grafico_temp_sensa <- function(df, titulo)
{
    media_verao_temp_sensa <- group_by(df, data)%>%summarise(TempMedia=mean(temp), SensaMedia=mean(sensa))
    ggplot(media_verao_temp_sensa, aes(x=data)) +
    geom_line(aes(y=TempMedia,colour = "Temperatura Média")) +
    geom_line(aes(y=SensaMedia, colour = "Sensação Termica Média")) +
    scale_colour_manual('', breaks = c('Temperatura Média', 'Sensação Termica Média'), values = c('red', 'blue')) +
    theme(legend.position = 'botton') +
    scale_x_date(NULL, date_labels <- "%b/%y", date_breaks <- "4 week") +
    theme(axis.text.x=element_text(angle = 60, hjust=1, size = 11, face = 'bold')) +
    ylab("Temperatura (C°)") +
    ggtitle(titulo)
}

grafico_temp_sensa(dados_verao_2015, 'Temperatura média e Sensação Térmica média durante o verão e o inverno de 2015')
grafico_temp_sensa(dados_verao_2016, 'Temperatura média e Sensação Térmica média durante o verão e o inverno de 2016')
grafico_temp_sensa(dados_verao_2017, 'Temperatura média e Sensação Térmica média durante o verão e o inverno de 2017')
grafico_temp_sensa(dados_verao_2018, 'Temperatura média e Sensação Térmica média durante o verão e o inverno de 2018')
grafico_temp_sensa(dados_verao_2019, 'Temperatura média e Sensação Térmica média durante o verão e o inverno de 2019')


#--------------------------------------------------------------#
#     3. Analise de dados                                      #
#--------------------------------------------------------------#
#     3.3 Analise vento temperatura verao inverno              #
#--------------------------------------------------------------#

cepagri_vt <- cepagri
cepagri_vt$dia<-getDay(cepagri_vt$horario)

#removendo na que podem ter sido colocado no tratamento de dados pois como a analise nao e temporal, interessa apenas os valores
cepagri_vt <- cepagri_vt[!is.na(cepagri_vt$temp), ]
cepagri_vt <- cepagri_vt[!is.na(cepagri_vt$vento), ]

#Para a escala de temperatura, foi utilizado valores padrao de frio abaixo de 19 graus, temperatura media entre 19 e 27 graus e calor acima de 27, muito calor acima de 31 graus
cepagri_vt$escala_temp <-  ifelse(cepagri_vt$temp > 19, ifelse(cepagri_vt$temp > 27, ifelse(cepagri_vt$temp > 31, "muito calor", "calor"), "normal"), "frio")

#Para os valores da escala de vento, foi utilizado os valores separados dos quartes dos dataset de verao, inverno e geral, fazendo uma media
cepagri_vt$escala_vento <-  ifelse(cepagri_vt$vento > 17, ifelse(cepagri_vt$vento > 36, "forte", "medio"), "fraco")
cepagri_vt_verao<-cepagri_vt[((cepagri_vt$mes==12&cepagri_vt$dia>=21) | (cepagri_vt$mes==1) | (cepagri_vt$mes==2) |  (cepagri_vt$mes==3 & cepagri_vt$dia<=20)),]
cepagri_vt_inverno<-cepagri_vt[((cepagri_vt$mes==6&cepagri_vt$dia>=21) | (cepagri_vt$mes==7) | (cepagri_vt$mes==8) |  (cepagri_vt$mes==9 & cepagri_vt$dia<=20)),]

#verao
summary(cepagri_vt_verao$vento)
summary(cepagri_vt_verao$inverno)

ggplot(cepagri_vt_verao, aes(x = temp, fill = escala_vento)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)+
theme_light() +
  labs(colour = element_blank(),
       title = "Temperatura e escala de vento no verao") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.9, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "Temperatura", limits = c(14, 36)) +
  scale_y_continuous(name = "Frequencia")

#analisando o grafico eh possivel ver que quando mais ocorrem ventos, as temperaturas ficam em torno de 20 a 25 graus e nas temperaturas
#altas, ocorrem uma quantidade menor de ventos de forma geral, prevalecendo ventos em velocidade media e pouquissimos ventos fortes no verao


ggplot(cepagri_vt_inverno, aes(x = temp, fill = escala_vento)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)+
  theme_light() +
  labs(colour = element_blank(),
       title = "Temperatura e escala de vento no inverno") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.9, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "Temperatura", limits = c(4, 36)) +
  scale_y_continuous(name = "Frequencia")


#no inverno, tambem ocorre com mais frequencia vento a temperatura entre 15 a 25 graus, sendo que entre 15 a 25 tem uma grande proporcao de ventos fortes
#nessa epoca do ano

ggplot(cepagri_vt, aes(x = temp, fill = escala_vento)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)
# considerando todos os meses dos anos analisados, a frequencia de ventos ocorre entre as temperaturas de 15 a 25 graus,
#sendo um pouco menor a ocorria de ventos fortes a temperatura acima de 25 graus diminuindo conforme a temperatura eleva.

#analisando de forma inversa, consideranto agora a escala da temperatura

ggplot(cepagri_vt_verao, aes(x = vento, fill = escala_temp)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)
#analisando o grafico eh possivel ver que quando mais ocorrem ventos, as temperaturas ficam em torno de 15 a 25 graus e conforme
# a velocidade do vendo maior, as temperaturas tendem a ficar no intervalo de 20 a 27 graus. Provalvemente ventos fortes
# amenizam a temperatura, pois como o dado analisado verao e esperado um valor alto de temperaturas calor e muito calor

ggplot(cepagri_vt_inverno, aes(x = vento, fill = escala_temp)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)
#no inverno, onde e esperado normalmente temperaturas mais baixas, e possivel ver que se a velocidade do vento esta alta,
#aparecem pouquissimos casos de temperatura calor

ggplot(cepagri_vt_verao, aes(x = vento, fill = escala_temp)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)+
  theme_light() +
  labs(colour = element_blank(),
       title = "Vento e temperatura Verao") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.9, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "Vento", limits = c(1, 77)) +
  scale_y_continuous(name = "Frequencia")



ggplot(cepagri_vt_inverno, aes(x = vento, fill = escala_temp)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)+
  theme_light() +
  labs(colour = element_blank(),
       title = "Vento e temperatura Inverno") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.9, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "Vento", limits = c(1, 77)) +
  scale_y_continuous(name = "Frequencia")



ggplot(cepagri_vt, aes(x = vento, fill = escala_temp)) + geom_histogram(color = "White", binwidth = 5, boundary = 0)+
  theme_light() +
  labs(colour = element_blank(),
       title = "Vento e temperatura geral") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.9, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "Vento", limits = c(1, 77)) +
  scale_y_continuous(name = "Frequencia")

  cepagri_vt_inverno$estacao<-"Inverno"
  cepagri_vt_verao$estacao<-"Verao"

  
  cepagri_vt_tabela  <- count(bind_rows(cepagri_vt_inverno[11:13],cepagri_vt_verao[11:13]), estacao, escala_temp, escala_vento)


  