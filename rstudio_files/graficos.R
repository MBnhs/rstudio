# install.packages("likert")
install.packages("readxl")

setwd("/home/rstudio/")
library(readxl)

bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd1_interesse")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_2")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 6] <- lapply(bd[ , 6], function(x){ factor(x, 
                                                       levels = c("1", "2", "3", "4", "5"),                                  
                                                       labels = c("SENT", "TA4HPD", "TA6HPD", "TA8HPD", "TM8HPD"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[6] <- paste(names(bd)[6], itens$texto, sep="_")
#======
library(likert)
#=========
# Colunas que contém as respotas das questões
lik <- likert(as.data.frame(bd[ , 6]))

#Plota as figuras
#O pacote espera que todas as variáveis sejam factor e utilizem a mesmma escala
# Opção 1
plot(lik, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="5"))
# Opção 2
plot(lik, type = "heat", wrap = 60, text.size=3.5) + theme(axis.text.y = element_text( size="8"))

#Compara opinião considerando diferentes tipos de deficiência (variável categ)
lik2 <- likert(as.data.frame(bd[ , 3:5]), grouping = bd$categ)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))

--------------------------------------------------------------------------------


bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd1_interesse")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_3")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 7] <- lapply(bd[ , 7], function(x){ factor(x, 
                                                 levels = c("0","1", "2", "3", "4", "5"),                                  
                                                 labels = c("NRT", "M1H", "E1e2H", "E2e3H", "E3e4H", "M4H"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[7] <- paste(names(bd)[7], itens$texto, sep="_")
#======
library(likert)
#=========
# Colunas que contém as respotas das questões
lik <- likert(as.data.frame(bd[ , 7]))

#Plota as figuras
#O pacote espera que todas as variáveis sejam factor e utilizem a mesmma escala
# Opção 1
plot(lik, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="5"))
# Opção 2
plot(lik, type = "heat", wrap = 60, text.size=3.5) + theme(axis.text.y = element_text( size="8"))

#Compara opinião considerando diferentes tipos de deficiência (variável categ)
lik2 <- likert(as.data.frame(bd[ , 3:5]), grouping = bd$categ)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))








------_____---------------------------------------------------------------------
  
bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd1_interesse")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_4")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 8:9] <- lapply(bd[ , 8:9], function(x){ factor(x, 
                                                 levels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),                                  
                                                 labels = c("0 Não gosta de jeito nenhum", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 Gosta muito"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[8:9] <- paste(names(bd)[8:9], itens$texto, sep="_")
#======
library(likert)
#=========
# Colunas que contém as respotas das questões
lik <- likert(as.data.frame(bd[ , 8:9]))

#Plota as figuras
#O pacote espera que todas as variáveis sejam factor e utilizem a mesmma escala
# Opção 1
plot(lik, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="8"))
# Opção 2
plot(lik, type = "heat", wrap = 60, text.size=3.5) + theme(axis.text.y = element_text( size="8"))

#Compara opinião considerando diferentes tipos de deficiência (variável categ)
lik2 <- likert(as.data.frame(bd[ , 3:5]), grouping = bd$categ)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))