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
setwd('/home/grgouveia/studies/mdc/INF-0612-I/trabalho-final')

install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)

# Filtra o dataframe (df) passado como argumento
# de acordo com um intervalo (interval) em ano
# ou mês, sendo estes critérios definidos pela
# variável attr. Os Valores possíveis para
# os atributos são mes e ano.

getYears <- function(col) {
  year <- as.POSIXlt(col)
  year <- unclass(year)$year + 1900
}

getMonths <- function(col) {
  month <- as.POSIXlt(col)
  month <- unclass(month)$mon + 1
}

filterBy <- function(attr, df, interval) {
  df$ano <-getYears(df$horario)
  df$mes <- getMonths(df$horario)

  df<-df[df$attr %in% interval,]
  
  print(df)
}

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
#     1.6 Análise registros duplicados                         #
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
#        Análise registros duplicados          #
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
#                                                              #
#---------------------------------------- ----------------------#
# Filtrando os dados agrupados por mês dentro do
# intervalo indicado
intervalo <- list(2015, 2016, 2017, 2018, 2019)
filterByYear <- filterBy("ano", cepagri, intervalo)
filterByMonth <- filterBy("mes", cepagri, intervalo)

cepagri$ano <- getYears(cepagri$horario)
cepagri$mes <- getMonths(cepagri$horario)

# Temperatura média de cada ano
temp_media_ano <- tapply(cepagri$temp, cepagri$ano, mean)

# Temperatura média de cada mês
temp_media <- tapply(cepagri$temp , cepagri$mes , mean)
temp_media <- round(temp_media)
temp_media


#-----------------------------------------------#
#     Medidas de posição com a base tratada     #
#-----------------------------------------------#
summary(cepagri)

##########################################################
##  O código do Boxplot pode ser melhorado e otimizado  ##
##########################################################
#boxplot
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

#? poss?vel observar que a sensa??o termica m?dia durante os meses sempre ? mais baixa que a temperatura m?dia. Nos
#meses de ver?o , 1,2,3 e 12, em que as temperaturas s?o mais altas e a velocidade de vento m?dia ? mais baixa,
#a diferen?a entre a sensa??o termica e a temperatura tende a ser menor.
#Nos meses de inverno, apresentou uma maior varia??o, principalmente quando a umidade media era um pouco mais baixa















