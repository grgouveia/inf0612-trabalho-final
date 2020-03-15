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

is_na <- function(df){
  any(is.na(df))
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
#--------------------------------------------------------------#
#     Processing data                                          #
#--------------------------------------------------------------#
# Removing lines with NA on any column
na_percent <- paste(sum(is.na(cepagri))/nrow(cepagri)*100,"%")
is_na <- apply(cepagri, 1, is_na)
cepagri <- cepagri[!is_na,];cepagri
confirm_removal <- any(is.na(cepagri)); paste("NA data", ifelse(!confirm_removal, "removed.", "removal failed."))

