#--------------------------------------------------------------#
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
setwd("~/studies/mdc/INF-0612-I/teste2")

# checa se o valor da linha é NA
is_na <- function(row){
  any(is.na(row))
}

# analisa se os dados nas k posições à frente e atràs da 
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

#cepagri <- read.csv("cepagri.csv", fill = TRUE, header = FALSE, sep = ";", col.names = names)

head(cepagri)
close(con)

#Observacao dos dados 
summary(cepagri)

#Filtrar pelos dados do enuncionado do trabalho, para isso criar as rowunas ano e mes e aplicar o filtro

cepagri$horario <- as.POSIXct(as.character(cepagri$horario), format = '%d/%m/%Y-%H:%M') 
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1
cepagri$dia <- unclass(cepagri$horario)$mday 

intervalo <- list(2015, 2016, 2017, 2018, 2019)
cepagri<-cepagri[cepagri$ano %in% intervalo,]


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
#     1.3 Corrigindo coerções implícitas indesejadas           #
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
#     1.3 Removendo outliers                                   #
#--------------------------------------------------------------#


###analisando discrepancia de cada informacao

## Removing outliers para sensa
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
#     1.4 Observações complementares                           #
#     Valores repetidos durante dias consectivos               #
#--------------------------------------------------------------#
cepagri[,1] <- as.POSIXct(
                as.character(cepagri[,1]),
                format = '%d/%m/%Y-%H:%M')
filtro <- consecutive(cepagri$temp, 144)
length(unique(as.Date(cepagri[filtro, 1])))


#umid
summary(cepagri$vento) 
sort(cepagri[cepagri$vento < 5,3])
#ocorrem valores proximos de 0, entao 0 parece um valor valido
#sobre o valor mais alto, 147, pesquisando na internet foi uma medicao verifica

#-----------------------------------------------#
#        Análise registros duplicados          #
#-----------------------------------------------#

#install.packages('tidyverse')
library(tidyverse)

#Retorna as linhas duplicadas do data frame
cepagri[duplicated(cepagri),]

# Exemplo de filtragem de uma linha duplidada
cepagri[cepagri$horario == '2015-01-23 09:24:00',]

#Remove linhas duplicadas
cepagri <- cepagri[!duplicated(cepagri),]

#verifica se ainda tem linhas duplicadas
cepagri[duplicated(cepagri),]


#-----------------------------------------------#
#       Início Análise dos dados                #
#-----------------------------------------------#

#Temperatura média de cada mês
temp_media <- tapply(cepagri$temp , cepagri$mes , mean)
temp_media <- round(temp_media)
temp_media


#----------------Medidas de posição com a base tratada
summary(cepagri)

library(ggplot2)

##########################################################
##  O código do Boxplot pode ser melhorado e otimizado  ##
##########################################################

#boxplot
cepagri2015 <- cepagri[cepagri$ano == 2015, ]
cepagri2016 <- cepagri[cepagri$ano == 2016, ]
cepagri2017 <- cepagri[cepagri$ano == 2017, ]
cepagri2018 <- cepagri[cepagri$ano == 2018, ]
cepagri2019 <- cepagri[cepagri$ano == 2019, ]


cepagri2015$mes <- as.factor(cepagri2015$mes)
cepagri2016$mes <- as.factor(cepagri2016$mes)
cepagri2017$mes <- as.factor(cepagri2017$mes)
cepagri2018$mes <- as.factor(cepagri2018$mes)
cepagri2019$mes <- as.factor(cepagri2019$mes)

ggplot(cepagri2015,
       aes(x = mes , y = temp , group = mes)) +
    geom_boxplot ()

ggplot(cepagri2016,
       aes(x = mes , y = temp , group = mes)) +
    geom_boxplot ()

ggplot(cepagri2017,
       aes(x = mes , y = temp , group = mes)) +
    geom_boxplot ()

ggplot(cepagri2018,
       aes(x = mes , y = temp , group = mes)) +
    geom_boxplot ()

ggplot(cepagri2019,
       aes(x = mes , y = temp , group = mes)) +
    geom_boxplot ()

#----------------Medidas de Dispersão

# Desvio padrão
dp <- c()
media <- c()
coef_var <- c()
for(i in 2:5){
    #Calculo desvio padrão para rowunas 2:5
    dp <- round(c(dp,sd(cepagri[,i],na.rm = TRUE)),2)
    #calculo média rowunas 2:5
    media <-round(c(media, mean(cepagri[,i],na.rm = TRUE)))
    
}
#Coeficiente de variação
coef_var <- round(c(coef_var, (dp/media)*100),2)
# Tabela que mostra a média, desvio padrão e coeficiente de variação de cada rowuna
variaveis <- c('temp','vento','umid','sensa')
medidas_dispersao <-data.frame(variaveis,media,dp,coef_var); medidas_dispersao


#-------------------------Histogramas
library(ggplot2)
library(dplyr)

# Histograma de cada rowuna 
hist(cepagri$temp, row = 'green', main = 'Histograma Temperatura', xlab = 'Temperatura', ylab = 'Frequência')
hist(cepagri$sensa, row = 'red', main = 'Histograma Sensação Térmica', xlab = 'sensação térmica', ylab = 'Frequência')
hist(cepagri$vento, row = 'gray', main = 'Histograma Vento', xlab = 'Vento', ylab = 'Frequência')
hist(cepagri$umid, row = 'blue', main = 'Histograma Umidade', xlab = 'Umidade', ylab = 'Frequência')


#-------------------------Analisando relacao temperatura umidade e sensacao termica
library(dplyr)
cepagri_analise <- cepagri

cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$temp), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$vento), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$sensa), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$umid), ]



dados_grafico2<-group_by(cepagri_analise, mes)%>%summarise(TempMedia=mean(temp), UmidMedia=mean(umid),   SensaMedi=mean(sensa), VentoMedi=mean(vento))


ggplot(dados_grafico2, aes(x = mes)) + 
  geom_point(aes(y = TempMedia, colour = "Temperatura media")) + 
  geom_line(aes(y = TempMedia, colour = "Temperatura media")) +
  
  geom_point(aes(y = UmidMedia, colour = "Umidade media")) + 
  geom_line(aes(y = UmidMedia, colour = "Umidade media")) +
  
  geom_point(aes(y = SensaMedi, colour = "Sensacao media")) + 
  geom_line(aes(y = SensaMedi, colour = "Sensacao media")) +
  
  geom_point(aes(y = VentoMedi, colour = "Velocidade Vento media")) + 
  geom_line(aes(y = VentoMedi, colour = "Velocidade Vento media")) +
  
  
  theme_light() +
  labs(colour = element_blank(), 
       title = "Comparativo dos valores medios ao mes de todo periodo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.1, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "mes", limits = c(1, 12),
                     breaks =  0:12,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Valor", limits = c(0, 100), 
                     breaks = 10 * 0:10,
                     minor_breaks = NULL)


dados_grafico2

#� poss�vel observar que a sensa��o termica m�dia durante os meses sempre � mais baixa que a temperatura m�dia. Nos
#meses de ver�o , 1,2,3 e 12, em que as temperaturas s�o mais altas e a velocidade de vento m�dia � mais baixa, 
#a diferen�a entre a sensa��o termica e a temperatura tende a ser menor. 
#Nos meses de inverno, apresentou uma maior varia��o, principalmente quando a umidade media era um pouco mais baixa 















