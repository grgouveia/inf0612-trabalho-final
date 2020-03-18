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

# Removing outliers
summary(cepagri)
cepagri[cepagri$sensa == 99.9, 5] <- NA
