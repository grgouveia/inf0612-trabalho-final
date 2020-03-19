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

consecutive_before <- function(vector, k = 1) {
  n <- length(vector)

  result <- logical(n)
  
  for (i in (1+k):n)
   if (all(vector[(i-k):(i-1)] == vector[i]))
     result[i] <- TRUE
  
   return(result)
}

consecutive_after <- function(vector, k = 1) {
  n <- length(vector)
  
  result <- logical(n)
  
  for (i in (1+k):n)
    if (all(vector[(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
  
  return(result)
}


consecutive_both <- function(vector, k = 1) {
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
#     1. Processing data                                       #
#--------------------------------------------------------------#
#     1.1 Loading data                                         #
#--------------------------------------------------------------#
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv", "r")
cepagri <- read.csv(con, header = FALSE,
                    sep = ";",
                    fill = TRUE,
                    col.names = names)
head(cepagri)
close(con)
#--------------------------------------------------------------#
#     1. Processing data                                       #
#--------------------------------------------------------------#
#     1.2 Removing lines with NA on any column                 #
#--------------------------------------------------------------#
na_percent <- paste(sum(is.na(cepagri))/nrow(cepagri)*100,"%")
if (na_percent > 0) {
  is_na <- apply(cepagri, 1, is_na)
  cepagri <- cepagri[!is_na,];cepagri
  confirm_removal <- !any(is.na(cepagri)); paste("Rows with NA data", ifelse(confirm_removal, "removed.", "removal failed."))
}

#--------------------------------------------------------------#
#     1. Processing data                                       #
#--------------------------------------------------------------#
#     1.3 Fixing mistakenly coerced columns                    #
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
#     1. Processing data                                       #
#--------------------------------------------------------------#
#     1.3 Removing outliers                                    #
#--------------------------------------------------------------#
summary(cepagri)
cepagri[cepagri$sensa == 99.9, 5] <- NA
summary(cepagri)

#--------------------------------------------------------------#
#     1. Processing data                                       #
#--------------------------------------------------------------#
#     1.4 Repeated values within consecutive days              #
#--------------------------------------------------------------#
cepagri[,1] <- as.POSIXct(
                as.character(cepagri[,1]),
                format = '%d/%m/%Y-%H:%M')
filtro <- consecutive(cepagri$temp, 144)
length(unique(as.Date(cepagri[filtro, 1])))