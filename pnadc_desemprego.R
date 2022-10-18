library(PNADcIBGE)
library(tidyverse)
library(dplyr)
library(reshape2)
library(readxl)
library(writexl)
library(survey)

options(scipen = 100) # para desabilitar a notação científica #
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
memory.limit(size=56000)

fazendo_desemp <-function(input) {
  # Construção das variáveis principais
  input <- input %>% mutate(desemp=ifelse(VD4002==2,1,ifelse(VD4002==1,0,NA)))
  output <- input %>% summarise(Desemprego=mean(desemp,na.rm=T)*100) 
  return(output)
}

fazendo_tabela <- function(t1,t2, t3) {
  r1 <- fazendo_desemp(t1)
  rfinal <- as.data.frame(r1$Desemprego)
  rfinal$Periferia <- if (nrow(t2) != 0) {fazendo_desemp(t2)$Desemprego} else {NaN}
  rfinal$Interior <- if (nrow(t3) != 0) {fazendo_desemp(t3)$Desemprego} else {NaN}
  rfinal$Estado <- unique(PNADC_T$UF)
  rfinal$Ano <- unique(PNADC_T$Ano)
  return(rfinal)
}

resfinal <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Capital", "Periferia", "Interior",'Ano','Estado'))
conta <- 0
# Lista de PNADCs
lista = c("Data/PNADC_042012.txt","Data/PNADC_042013.txt","Data/PNADC_042014.txt","Data/PNADC_042015.txt",
          "Data/PNADC_042016.txt","Data/PNADC_042017.txt","Data/PNADC_042018.txt","Data/PNADC_042019.txt",
          "Data/PNADC_042020.txt","Data/PNADC_042021.txt")
estados = unique(read_pnadc(microdata = 'Data/PNADC_042021.txt', input_txt="Data/Input_PNADC_trimestral.txt", 
                            vars= c("UF"))$UF)
for (i in lista){
  # Lendo o excel
  PNADC <- read_pnadc(microdata = i, input_txt="Data/Input_PNADC_trimestral.txt", 
                      vars = c("UF","Ano",'Capital','RM_RIDE','V2009', 'VD4002'))
  PNADC <- PNADC %>% filter(V2009>15) #apenas pop economicamente ativa
  # Filtrar para SP
  for (j in estados) {
    PNADC_T <- PNADC %>% filter(UF==j) 
    PNADC_T1 <- PNADC_T %>% filter(!is.na(Capital))
    PNADC_T2 <- PNADC_T %>% filter(!is.na(RM_RIDE)) %>% filter(is.na(Capital))
    PNADC_T3 <- PNADC_T %>% filter(is.na(RM_RIDE)) %>% filter(is.na(Capital))
    restemp <- fazendo_tabela(PNADC_T1, PNADC_T2, PNADC_T3)
    resfinal <- rbind(resfinal,restemp)
    conta <- conta +1
    if (conta %% 27 == 0) {
      print(paste(conta/27, "PNADC de", length(lista), "PNADCs", sep=" "))
    }
  }
}

# Salvando resultado
names(resfinal)[names(resfinal) == 'r1$Desemprego'] <- 'Capital'
write.csv(resfinal, 'pnadc_desemprego.csv', row.names = FALSE)
