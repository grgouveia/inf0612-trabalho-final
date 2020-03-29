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
#     Configuracao dos arquivos, libs e funções auxiliares     #
#--------------------------------------------------------------#
install.packages('tidyverse')
install.packages("DT")
install.packages("systemfonts")
install.packages("gdtools")
install.packages("hrbrthemes")
install.packages("viridis")
library(viridis)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

gbarplot <- function(df) {
  ggplot(df, 
      aes(fill=periodo, x=ano, y=Vento)) +
      geom_bar(position=position_dodge(width=0.6), stat="identity", width=0.5) +
      #scale_fill_viridis(discrete = T) + 
      ggtitle("Velocidade do vento por período do dia") +
      theme_ipsum() +
      xlab("Ano") +
      ylab("Velocidade do Vento (km/h)") 
}

# extrai hora de um Date em POSIXct
getHour <- function(time) {
  time <- as.POSIXlt(time);
  unclass(time)$hour
}

# retorna periodo do dia de acordo com a hora
getPeriod <- function(hour){
  if (hour > 0 & hour < 6) return("1 - madrugada")
  else if (hour >= 6 & hour < 12) return("2 - manhã")
  else if (hour >= 12 & hour < 18) return("3 - tarde")
  else return("4 - noite")
}


# Filtra o dataframe (df) passado como argumento
# de acordo com um intervalo (interval) em ano
# ou mÃªs, sendo estes critérios definidos pela
# variÃ¡vel attr. Os Valores possíveis para
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

# analisa se os dados nas k posições à frente e atrás da
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
#     1. Tratando os  dados                                    #
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
#     1. Processando os dados                                  #
#--------------------------------------------------------------#
#     1.2 Corrigindo coerções implícitas indesejadas           #
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
#     1. Processando os dados                                  #
#--------------------------------------------------------------#
#     1.3 Convertendo coluna de data p/ POSIXct
#--------------------------------------------------------------#
cepagri$horario <- as.POSIXct(
                as.character(cepagri$horario),
                format='%d/%m/%Y-%H:%M')

#--------------------------------------------------------------#
#     1. Processando os dados                                  #
#--------------------------------------------------------------#
#     1.4 Removendo linhas com valor NA                        #
#--------------------------------------------------------------#
na_percent <- paste(sum(is.na(cepagri))/nrow(cepagri)*100,"%")
if (na_percent > 0) {
  is_na <- apply(cepagri, 1, is_na)
  cepagri <- cepagri[!is_na,];cepagri
  confirm_removal <- !any(is.na(cepagri)); paste("Rows with NA data", ifelse(confirm_removal, "removed.", "removal failed."))
}


#--------------------------------------------------------------#
#     1. Processando os dados                                  #
#--------------------------------------------------------------#
#     1.5 Removendo outliers                                   #
#--------------------------------------------------------------#
summary(cepagri)

#e possivel ver pelo summary que existem duas medidas que sao outliers, a sensacao termica de 99.9
#e a umidade de zero. Abaixo executando o boxplot e hsitogramas sem esse tratamento fica mais evidente ainda
#nos dois casos, a solucao escolhida foi deixar o valor com Nas pois os outros registros sao registros validos
## removendo outliers
#sensa
summary(cepagri$sensa)
cepagri[cepagri$sensa == 99.9, 5] <- NA

#umid
cepagri[cepagri$umid == 0, 4] <- NA

summary(cepagri)
#confirmando que apenas as colunas esperadas foram colocadas na
cepagri[is.na(cepagri$sensa), ]
cepagri[is.na(cepagri$umid), ]

#--------------------------------------------------------------#
#     1. Processando os dados                                  #
#--------------------------------------------------------------#
#     1.6 Análise de registros duplicados                      #
#         Valores repetidos durante dias consectivos           #
#--------------------------------------------------------------#
# filtra os valores recorrentes em 144 dias consecutivos
filtro <- consecutive(cepagri$temp, 144)
length(unique(as.Date(cepagri[filtro, 1])))


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
#     2. Analise exploratória dos dados                        #
#--------------------------------------------------------------#
#                                                              #
#     2.1 Medidas de posição com a base tratada                #
#--------------------------------------------------------------#
summary(cepagri)

#--------------------------------------------------------------#
#     2. Analise exploratória dos dados                        #
#--------------------------------------------------------------#
##                                                             #
#     2.1 Medidas de posição com a base tratada                #
#--------------------------------------------------------------#
#---------------------- Boxplot
boxplot <- function(variavel,titulo)
{
  title <- paste(titulo)
  plot <- ggplot(cepagri,
            aes(y = variavel)) +
            geom_boxplot () +
            theme_light() +
            labs(colour = element_blank(),
                title = title) +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position = c(0.5, 0.15)) +
            theme(legend.box.background = element_rect(colour = "black"))
  return(plot)
}  



boxplot(cepagri$temp, "boxplot Temperatura")
boxplot(cepagri$sensa, "boxplot Sensação Térmica")
boxplot(cepagri$vento, "boxplot Vento")
boxplot(cepagri$umid, "boxplot Umidade")

#--------------------------Desvio padrão
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


#--------------------------------------------------------------#
#     3. Analisando dados                                      #
#--------------------------------------------------------------#
#     3.1 Agrupando dados por mês e ano                        #
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
#     3. Analisando dados                                      #
#--------------------------------------------------------------#
#                                                              #
#     3.2 Análise de dados por períodos                        #
#     classifica cada medição de acordo com o período do dia   #
#     (madrugada, manhã, tarde, noite)                         #
#--------------------------------------------------------------#
intervalo <- list(2015, 2016, 2017, 2018, 2019)
periodos <- c("1 - manhã", "2 - tarde", "3 - noite", "4 - madrugada")

# cria coluna de hora para identificar o período
cepagri$hora <- getHour(cepagri$horario)
# agrupa medições em períodos
cepagri$periodo <- as.character(lapply(cepagri$hora, getPeriod))
ventoPorPeriodoEAno <- as.data.frame(group_by(cepagri, periodo, ano)%>%summarise(TempMedia=mean(temp), SensaMedia=mean(sensa), Vento=mean(vento)))
# remove anos que contém dados desbalanceados (2014 e 2020)
ventoPorPeriodoEAno[ventoPorPeriodoEAno$ano %in% intervalo, ]
# exibe dados em gráficos de barra agrupados por período
gbarplot(ventoPorPeriodoEAno)


###############################################################
#-------------------------------------------------------------#
#     3. Finalizar criação de tabeas!!!!                      #
#     TODO                                                    #
#-------------------------------------------------------------#
################################################################
# cria tabela para o modek
# filtra e armazena em listas as medições de acordo com os períodos
cepagri_periodos <- list()
for (i in 1:length(periodos)) {
  cepagri_periodos[i] <- list(subset(cepagri, periodo == periodos[i]))
}
summaryByDayPeriod <- list()
for (i in 1:length(cepagri_periodos)) {
    summaryByDayPeriod[i] <- list(group_by(cepagri_periodos[[i]], ano)%>%summarise(Vento=mean(vento)))
}

#--------------------------------------------------------------#
#     3. Analisando dados                                      #
#--------------------------------------------------------------#
#     3.4 Analise das estacoes verao e inverno                 #
#--------------------------------------------------------------#

cepagri_analise <- cepagri

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

grafico_temp_sensa(dados_verao_2015, 'Temperatura média e Sensação Térmica média durante o verão de 2015')
grafico_temp_sensa(dados_verao_2016, 'Temperatura média e Sensação Térmica média durante o verão de 2016')
grafico_temp_sensa(dados_verao_2017, 'Temperatura média e Sensação Térmica média durante o verão de 2017')
grafico_temp_sensa(dados_verao_2018, 'Temperatura média e Sensação Térmica média durante o verão de 2018')
grafico_temp_sensa(dados_verao_2019, 'Temperatura média e Sensação Térmica média durante o verão de 2019')



#--------------------------------------------------------------#
#     3. Analisando dados                                      #
#--------------------------------------------------------------#
#     3.X Comparativo entre as medidas                         #
#--------------------------------------------------------------#
cepagri_analise <- cepagri
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$temp), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$vento), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$sensa), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$umid), ]
cepagri_analise$diferenca_temp_sensa <- cepagri_analise$temp-cepagri_analise$sensa


intervalo <- list(2015, 2016, 2017, 2018, 2019)
cepagri_analise<-cepagri_analise[cepagri_analise$ano %in% intervalo,]

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




#--------------------------------------------------------------#
#     3. Analisando dados                                       #
#--------------------------------------------------------------#
#     3.X Analise vento temperatura verao inverno              #
#--------------------------------------------------------------#

cepagri_vt <- cepagri


intervalo <- list(2015, 2016, 2017, 2018, 2019)
cepagri_vt<-cepagri_vt[cepagri_vt$ano %in% intervalo,]

cepagri_vt$horario<-NULL
cepagri_vt$umid<-NULL
cepagri_vt$sensa<-NULL
cepagri_vt$hora<-NULL
cepagri_vt$periodo<-NULL

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

cepagri_vt$estacao <-  ifelse(cepagri_vt$mes == 1 | cepagri_vt$mes ==2, "Verao",
                              ifelse(cepagri_vt$mes == 12 , ifelse (cepagri_vt$dia>=21, "Verao", "Primavera"),
                                     ifelse(cepagri_vt$mes == 3 , ifelse (cepagri_vt$dia<21, "Verao", "Outono"),
                                            ifelse(cepagri_vt$mes == 4 | cepagri_vt$mes ==5, "Outono",
                                                   ifelse(cepagri_vt$mes == 6 , ifelse (cepagri_vt$dia>=21, "Inverno", "Outono"),
                                                          ifelse(cepagri_vt$mes == 7 | cepagri_vt$mes ==8, "Inverno",
                                                                 ifelse(cepagri_vt$mes == 9 , ifelse (cepagri_vt$dia>=21, "Primavera", "Inverno"), "Primavera")#mes 9
                                                          )#mes 7 e 8
                                                   )#mes 6
                                            )#mes 4 e 4
                                     )#mes 3       
                              )#mes 12
)#mes 1 e 2



  
  cepagri_vt_tabela <-   
    group_by(cepagri_vt,  estacao, escala_temp, escala_vento)%>%count()
  
  names_grafico <- c("Estacao", "Temperatura", "Vento", "Frequencia")
  
  names(cepagri_vt_tabela) <- names_grafico
  
  cepagri_vt_tabela[cepagri_vt_tabela$Estacao=="Inverno"|cepagri_vt_tabela$Estacao=="Verao",]
  