# install.packages("likert")
install.packages("readxl")

setwd("/home/rstudio/")
library(readxl)
bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd3")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 3:12] <- lapply(bd[ , 3:12], function(x){ factor(x, 
                                                       levels = c("0","1", "2", "3", "4", "5"),                                  
                                                       labels = c("NS","DT", "D", "N", "C", "CT"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[3:12] <- paste(names(bd)[3:12], itens$texto, sep="_")
#======
library(likert)
#=========
# Colunas que contém as respotas das questões
lik <- likert(as.data.frame(bd[ , 3:12]))
liksexo <- likert(as.data.frame(bd[ , 3:13]))
#Plota as figuras
#O pacote espera que todas as variáveis sejam factor e utilizem a mesmma escala
# Opção 1
plot(lik, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="10"))
# Opção 2
plot(lik, type = "heat", wrap = 60, text.size=2) + theme(axis.text.y = element_text( size="10"))

#Compara opinião considerando diferentes tipos de deficiência (variável categ)
lik2 <- likert(as.data.frame(bd[ , 3:12]), grouping = bd$categ)
lik2 <- likert(as.data.frame(bd[ , 3:5]), grouping = bd$sexo)
plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))
----------------------------------------------------------------------------------------------

bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd1_interesse")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_2")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 3:4] <- lapply(bd[ , 3:4], function(x){ factor(x, 
                                                       levels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),                                  
                                                       labels = c("NGL","1", "2", "3", "4", "5", "6", "7", "8", "9", "GML"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[3:4] <- paste(names(bd)[3:4], itens$texto, sep="_")

likinteresse <- likert(as.data.frame(bd[ , 3:4]))


# Opção 1
plot(likinteresse, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="5"))
# Opção 2
plot(likinteresse, type = "heat", wrap = 60, text.size=2) + theme(axis.text.y = element_text( size="10"))

-----------------------------------------------------------------------------------
  
  
bd <- read_excel("bd_Usabilidade.xlsx", sheet = "bd1_interesse")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_2")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bd[ , 5] <- lapply(bd[ , 5], function(x){ factor(x, 
                                                     levels = c("1", "2", "3"),                                  
                                                     labels = c("Feminino", "Masculino", "Nao quis informar"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bd)[5] <- "Genero"

likinteresse <- likert(as.data.frame(bd[ , 5]))

bd


# Opção 1
plot(likinteresse, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="5"))
# Opção 2
plot(likinteresse, type = "heat", wrap = 60, text.size=2) + theme(axis.text.y = element_text( size="10"))


___________________________________________________________

bdidade <- read_excel("bd_Usabilidade.xlsx", sheet = "bd_idade")
itens <- read_excel("bd_Usabilidade.xlsx", sheet = "itens_2")

#---- ORGANIZA O BANCO DE DADOS
# Transforma em factor
# Colunas que contém as respotas das questões
bdidade[ , 2] <- lapply(bdidade[ , 2], function(x){ factor(x, 
                                                 levels = c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67"),                                  
                                                 labels = c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67"))})

# Muda os nomes das variáveis
# Colunas que contém as respotas das questões
names(bdidade)[2] <- "idade"

likinteresse <- likert(as.data.frame(bdidade[ , 2]))

bd


# Opção 1
plot(likinteresse, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="5"))
# Opção 2
plot(likinteresse, type = "heat", wrap = 60, text.size=2) + theme(axis.text.y = element_text( size="10"))


lik2 <- likert(as.data.frame(bdidade[ , 2]), grouping = bdidade$genero)

plot(lik2, wrap = 60, text.size=3) + theme(axis.text.y = element_text(size="6"))


formatos <- theme(
  plot.title=element_text(size = 14, hjust = 0.5),
  axis.title.y=element_text(size = 12, vjust = +0.2),
  axis.title.x=element_text(size = 12, vjust = -0.2),
  axis.text.y=element_text(size = 10),
  axis.text.x=element_text(size = 10)
)
ggplot(bdidade$idade, aes(x = Idade)) + 
  geom_histogram(bins = 50) + 
  ylab("Frequência") + 
  xlab("Idades") + 
  ggtitle('Histograma das Idades') +
  formatos


bdidade


bdidade <- read_excel("bd_Usabilidade.xlsx", sheet = "bd_idade")
ggplot(bdidade, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ylab("Frequência") +
  xlab("Valores") +
  ggtitle("Histograma de Valores de Exemplo")


ggplot(bdidade, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ylab("Frequência") +
  xlab("Valores") +
  ggtitle("Histograma de Valores") +
  scale_x_discrete(breaks = unique(bdidade$idade))



ggplot(bdidade, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ylab("Quantidade") +
  xlab("PI.Q2 - Idade") +
  ggtitle("Histograma de idades dos participantes") +
  scale_x_continuous(breaks = unique(bdidade$idade))



ggplot(bdidade, aes(x = genero1, fill =)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ylab("Quantidade") +
  xlab("Idades") +
  ggtitle("Histograma de idades dos participantes") +
  scale_x_continuous(breaks = unique(bdidade$genero1))

bdidade



ggplot(bdidade, aes(x = genero1, fill = genero1)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_fill_manual(values = c("Feminino" = "blue", "Masculino" = "red", "Não quis informar" = "green"),
                    labels = c("Feminino", "Masculino", "Não quis informar")) +
  ylab("Quantidade") +
  xlab("Idades") +
  ggtitle("Histograma de Idades dos Participantes") +
  theme(legend.title = element_blank()) 



ggplot(bdidade, aes(x = genero1, fill = genero1)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_discrete(labels = c("Feminino" = "Masculino", "Masculino" = "Feminino", "Não quis informar" = "Não quis informar")) +
  ylab("Quantidade") +
  xlab("Idades") +
  ggtitle("Histograma de Idades dos Participantes") +
  theme(legend.title = element_blank())  # Remover o título da legenda


ggplot(bdidade, aes(x = genero1)) +
  geom_bar(fill = "blue") +
  scale_x_discrete(labels = c("Masculino", "Feminino", "Não quis informar")) +
  ylab("Quantidade") +
  xlab("Gênero") +
  ggtitle("Gráfico de Barras do Gênero dos Participantes")



ggplot(bdidade, aes(x = genero)) +
  geom_bar() +
  ylab("Frequência") +
  xlab("Categorias") +
  ggtitle("Gráfico de Barras das Categorias")

bdidade <- read_excel("bd_Usabilidade.xlsx", sheet = "bd_idade")

cores <- c("Feminino" = "red", "Masculino" = "blue", "Não quis informar" = "darkgray")

ggplot(bdidade, aes(x = genero, fill = genero)) +
  geom_bar() +
  scale_fill_manual(values = cores) +  # Atribua cores personalizadas
  ylab("Frequência") +
  xlab("Categorias") +
  ggtitle("Gráfico de Barras das Categorias")




ggplot(bdidade, aes(x = Gênero, y = ..count.., fill = Gênero)) +
  geom_bar(stat = "count", position = position_stack(), color = "white") +
  scale_fill_manual(values = cores) +
  ylab("Quantidade") +
  xlab("PI.Q1 - Gênero") +
  ggtitle("Gráfico de Barras de gêneros dos participantes") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 4, color = "white")