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
#setwd("C:\\Projetos\\Unicamp\\Trabalhos\\INF-0612-2020\\TrabalhoFinal")

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



#-------------------------Analisando relacao temperatura umidade 



cepagri_temp_umid_sensa <- cepagri[2:5]
cepagri_temp_umid_sensa[2] <- NULL
cepagri_temp_umid_sensa[duplicated(cepagri_temp_umid_sensa),]

#Remove linhas duplicadas
cepagri_temp_umid_sensa <- cepagri_temp_umid_sensa[!duplicated(cepagri_temp_umid_sensa),]


#verifica se ainda tem linhas duplicadas
cepagri_temp_umid_sensa[duplicated(cepagri_temp_umid_sensa),]

p_teste <- ggplot(cepagri_temp_umid_sensa, aes(x = temp,  y = umid,  group = temp)) 
p_teste$layers 
p_teste <- p_teste + geom_point() + geom_line() 
p_teste$layers



#-------------------------Analisando relacao temperatura umidade e sensacao termica
library(dplyr)
#temp
#temp dados mininos e maximos-- encont5rando dados medios por dia e arredondando o valor
cepagri_analise <- cepagri

cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$temp), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$vento), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$sensa), ]
cepagri_analise <- cepagri_analise[!is.na(cepagri_analise$umid), ]


formar_grupos<-function(valor, conjunto, intervalo, limite)
{
  if(valor > limite)
  {
    return(limite)
  }
  
  if(valor <= intervalo)
  {
    return (intervalo)
  }
  else
  {
    intervalo<-intervalo+conjunto
    return(formar_grupos(valor, conjunto, intervalo, limite))
  }
}

categoria<-function(valor)
{
  if(valor > 1)
  {
    return(2)
  }
  
  if(valor < 1)
  {
    return (0)
  }
  else
  {
    return(1)
  }
}


dados_grafico2<-group_by(cepagri_analise, mes)%>%summarise(TempMedia=mean(temp), UmidMedia=mean(umid),   SensaMedi=mean(sensa))

#dados_grafico$TempMedia<-round(dados_grafico$TempMedia)
#dados_grafico$UmidMedia<-round(dados_grafico$UmidMedia)

#summary(dados_grafico$SensaMedi)
#dados_grafico$SensaGraf<- sapply(dados_grafico$SensaMedi,formar_grupos, conjunto=2, intervalo = 2, limite = 40)

#summary(dados_grafico$TempMedia)
#dados_grafico$TempGraf<- sapply(dados_grafico$TempMedia,formar_grupos, conjunto=2, intervalo = 2, limite = 40)

#summary(dados_grafico$UmidMedia)
#dados_grafico$UmidGraf<- sapply(dados_grafico$UmidMedia,formar_grupos, conjunto=2, intervalo = 2, limite = 100)

dados_grafico<-cepagri_analise
summary(cepagri_analise$sensa)
dados_grafico$SensaGraf<- sapply(cepagri_analise$sensa,formar_grupos, conjunto=3, intervalo = 3, limite = 40)

summary(cepagri_analise$temp)
dados_grafico$TempGraf<- sapply(cepagri_analise$temp,formar_grupos, conjunto=3, intervalo = 3, limite = 40)

summary(cepagri_analise$vento)
dados_grafico$VentoGraf<- sapply(cepagri_analise$vento,formar_grupos, conjunto=10, intervalo = 10, limite = 150)

summary(cepagri_analise$umid)
dados_grafico$UmidGraf<- sapply(cepagri_analise$umid,formar_grupos, conjunto=5, intervalo = 5, limite = 100)

dados_grafico$CorGraf<- dados_grafico$TempGraf/dados_grafico$SensaGraf
dados_grafico$CorGraf<- sapply(dados_grafico$CorGraf,categoria)


dados_grafico<-dados_grafico[9:13]
dados_grafico<-dados_grafico[!duplicated(dados_grafico),]


#ggplot(dados_grafico[!duplicated(dados_grafico),],  aes(x = TempMedia, y = UmidMedia,   size = (SensaMedAum), colour = 'color'))   + geom_point(alpha = 0.5)


dados_grafico$CorGraf <- as.factor(dados_grafico$CorGraf)

ggplot(dados_grafico,  aes(x = TempGraf, y = VentoGraf,   size = UmidGraf, colour = dados_grafico$CorGraf))   + geom_point() 
  



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
       title = "Comparativo dos valores medios") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.1, 0.9)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(name = "mes", limits = c(1, 12),
                     breaks =  0:12,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Valor", limits = c(0, 100), 
                     breaks = 10 * 0:10,
                     minor_breaks = NULL)


















