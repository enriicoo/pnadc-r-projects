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

fazendo_quantis <-function(input) {
  # Juntar PNADC_T com Deflatores
  input <- merge(input,Deflator,by=c("Ano","UF"))
  
  # Construção das variáveis principais
  input <- input %>% mutate(RendaTrab=ifelse(VD4002==1 & is.na(VD4019)==F & VD4019<=999999, VD4019*Habitual, ifelse(VD4002==2,0,NA)),
                                    QlqrRendaTrab=ifelse(is.na(RendaTrab)==T,0,RendaTrab),
                                    id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                                    pessoa= 1)
  
  # Construir variável de renda domiciliar e número de indivíduos no domicílio
  input <- input %>% group_by(id_dom) %>% mutate(n_ind=sum(pessoa),
                                                         RTD=sum(QlqrRendaTrab))
  # Renda Domiciliar Per Capita
  input <- input %>% mutate(RTDpc=RTD/n_ind)
  
  # Criar base de dados para ler a PNADC como uma base amostral complexa 
  dstrat1 <- svydesign(id=~UPA, strata=~Estrato, weights=~V1028, data=input)
  
  # Calcular Renda Domiciliar per Capita 
  vetor = c(seq(0.005, 1, by=0.01))
  RTDOMpcmed <- svyquantile(~RTDpc, quantiles=vetor, dstrat1, na.rm = T)
  output <- as.data.frame(RTDOMpcmed$RTDpc)
  return(output)
}

fazendo_tabela <- function(t1,t2, t3) {
  r1 <- fazendo_quantis(t1)
  rfinal <- as.data.frame(r1$quantile)
  rfinal$Periferia <- if (nrow(t2) != 0) {fazendo_quantis(t2)$quantile} else {c(1:100)*NaN}
  rfinal$Interior <- if (nrow(t3) != 0) {fazendo_quantis(t3)$quantile} else {c(1:100)*NaN}
  rfinal$Estado <- unique(PNADC_T$UF)
  rfinal$Ano <- unique(PNADC_T$Ano)
  rfinal$Quantil <- c(seq(0.01, 1, by=0.01))
  return(rfinal)
}

resfinal <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Capital", "Periferia", "Interior",'Ano','Estado'))
conta <- 0
# Lista de PNADCs
lista = c("Data/PNADC_042012.txt","Data/PNADC_042013.txt","Data/PNADC_042014.txt","Data/PNADC_042015.txt",
          "Data/PNADC_042016.txt","Data/PNADC_042017.txt","Data/PNADC_042018.txt","Data/PNADC_042019.txt",
          "Data/PNADC_042020.txt","Data/PNADC_042021.txt")
Deflator <- read_excel("Data/deflator_PNADC_2022_1.xls",sheet="deflator")
estados = unique(read_pnadc(microdata = 'Data/PNADC_042021.txt', input_txt="Data/Input_PNADC_trimestral.txt", 
                            vars= c("UPA", "UF"))$UF)
for (i in lista){
  # Lendo o excel
  PNADC <- read_pnadc(microdata = i, input_txt="Data/Input_PNADC_trimestral.txt", 
                        vars= c("UPA", "V1028", "UF","Ano",'Capital','RM_RIDE','V1022','V1023','V2001','V2007','V2009','V2010','VD4020','UPA','V1008','V1014','VD4019','VD4002'))
  # Filtrar para SP
  for (j in estados) {
    PNADC_T <- PNADC %>% filter(UF==j) %>% filter(V2009>15) #apenas pop economicamente ativa
    PNADC_T1 <- PNADC_T %>% filter(!is.na(Capital))
    PNADC_T2 <- PNADC_T %>% filter(!is.na(RM_RIDE)) %>% filter(is.na(Capital))
    PNADC_T3 <- PNADC_T %>% filter(is.na(RM_RIDE)) %>% filter(is.na(Capital))
    restemp <- fazendo_tabela(PNADC_T1,PNADC_T2, PNADC_T3)
    resfinal <- rbind(resfinal,restemp)
    conta <- conta +1
    if (conta %% 27 == 0) {
    print(paste(conta/27, "PNADC de", length(lista), "PNADCs", sep=" "))
    }
  }
}

# Salvando resultado
names(resfinal)[names(resfinal) == 'r1$quantile'] <- 'Capital'
write.csv(resfinal, 'pnadc_rdpc.csv', row.names = FALSE)