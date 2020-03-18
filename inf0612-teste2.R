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

is_na <- function(col){
  any(is.na(col))
}
#--------------------------------------------------------------#
#     Loading data                                             #
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

#Filtrar pelos dados do enuncionado do trabalho, para isso criar as colunas ano e mes e aplicar o filtro

cepagri$horario <- as.POSIXct(as.character(cepagri$horario), format = '%d/%m/%Y-%H:%M') 
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1

intervalo <- list(2015, 2016, 2017, 2018, 2019)
cepagri<-cepagri[cepagri$ano %in% intervalo,]


#--------------------------------------------------------------#
#     Processing data                                          #
#--------------------------------------------------------------#
# Removing lines with NA on any column
na_percent <- paste(sum(is.na(cepagri))/nrow(cepagri)*100,"%")
if (na_percent > 0) {
  is_na <- apply(cepagri, 1, is_na)
  cepagri <- cepagri[!is_na,];cepagri
  confirm_removal <- !any(is.na(cepagri)); paste("Rows with NA data", ifelse(confirm_removal, "removed.", "removal failed."))
}

# Fixing mistakenly coerced columns
for (i in 2:length(cepagri)) {
  aux <- cepagri[,i]
  if(is.factor(aux)) {
    aux <- as.character(aux)
    aux <- as.numeric(aux)
  }
  print(class(aux))
  cepagri[,i] <- aux
}

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


#umid
summary(cepagri$vento) 
sort(cepagri[cepagri$vento < 5,3])
#ocorrem valores proximos de 0, entao 0 parece um valor valido
#sobre o valor mais alto, 147, pesquisando na internet foi uma medicao verifica


## Analisar registros duplicados
