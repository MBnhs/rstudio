options(repos = "http://cran-r.c3sl.ufpr.br/")
# sessionInfo()
# dados <- read.csv("dados.csv")
participantes <- read.csv("/home/rstudio/participantes.csv")

# head(participantes, 5)


# install.packages("dplyr")
# library(dplyr)



# c(arrange(unique(select(participantes,idade)), idade))

# chooseCRANmirror()

# install.packages("glue")
# library(glue)

print("Distribuição de frequências para variáveis qualitativas - Q1")
table(participantes$Q1)


print("Distribuição (percentual) de frequências para variáveis qualitativas - Q1")
# prop.table(table(participantes$Q1)) * 100


dist_freq_qualitativas <- cbind(freq = table(participantes$Q1), percent = prop.table(table(participantes$Q1)) * 100)

colnames(dist_freq_qualitativas) <- c("Frequencia", "Porcentagem (%)")
dist_freq_qualitativas


frequencia <- table(participantes$Q1, participantes$idade)

rownames(frequencia) <- c('Q10', 'Q11', 'Q12', 'Q13', 'Q14', 'Q15')


frequencia <- cbind(frequencia)
frequencia



# print("Media da idade dos participantes")

# mediaIdade <- tapply(participantes$idade, list(participantes$Q1, participantes$Q2, participantes$Q3), mean)
# mediaIdade


print ("Faixas etarias")
classes <- c(min(participantes$idade),30,40,50,60, max(participantes$idade))
labels <- c('Ate 30 anos', 'Entre 31 e 40 anos', 'Entre 41 e 50 anos', 'Entre 51 e 60 anos', 'Acima de 60 anos')

frequencia <- table (cut(
    x = participantes$idade,
    breaks = classes,
    labels = labels,
    include.lowest = TRUE
))
frequencia

n <- nrow(participantes)

k <- 1 + (10/3) * log10(n)

k <- round(k)
print("K redondo")
k
frequencia <- table (cut(
    x = participantes$idade,
    breaks = k,
    
    include.lowest = TRUE
))
frequencia

percentual <- prop.table(frequencia) * 100

print ("Distribuicao de frequencia quantitativas amplitude fixa")
dist_freq_quantitativas_amplitude_fixa <- cbind('Frequencia' = frequencia, 'Porcentagem (%)' = percentual)
dist_freq_quantitativas_amplitude_fixa


hist(participantes$idade)

hist(
  x = participantes$idade,
  breaks = 'Sturges',
  col = 'lightblue',
  main = 'Histograma das idades',
  xlab = 'Altura',
  ylab = 'Frequencias'
)

install.packages("ggplot2")

library(ggplot2)


ggplot(participantes, aes(x = idade)) +
  geom_histogram(binwidth = 0.02, color = "black", alpha = 1) +
  ylab("Frequencia") +
  xlab("Alturas") +
  ggtitle("Histograma das Alturas") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, vjust = +0.2),
    axis.title.x = element_text(size = 12, vjust = -0.2),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

formatos <- theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

ggplot(participantes, aes(x = idade, y = ..density..)) +
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) +
  geom_density(color = 'green') +
  ylab("Frequencia") +
  xlab("Alturas") +
  ggtitle("Histograma das Alturas") +
  formatos


bar_chart <- data.frame(dist_freq_quantitativas_amplitude_fixa)
bar_chart

ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Frequencia)) +
  geom_bar(stat = "identity") +
  ylab("Frequencia") +
  xlab("Classes de idade") +
  ggtitle("Grafico Classes de Idade") +
  formatos



print("------------------------------------------------------------")

print("Medidas de tendencia central")



materias <- c("Matematica", "Portugues", "Ingles", "Geografia", "Historia", "Fisica", "Quimica")
Marcelo <- c(8,10,4,8,6,10,8)
Katia <- c(10,2,0.5,1,3,9.5,10)
Cida <- c(7.5,8,7,8,8,8.5,7)

df <- data.frame(Marcelo, Katia, Cida, row.names = materias)
df


mean(df$Marcelo)

mean(participantes$idade)

aggregate(list(Renda = participantes$idade), list(Sexo = participantes$sexo), mean)


df_marcelo <- df[order(df$Marcelo), ]

n = nrow(df_marcelo)
n
elemento_md <- (n+1) /2
elemento_md

df_marcelo[elemento_md, ]


# Mediana do marcelo
median(df$Marcelo)


median(participantes$idade)

print("------------Moda------------")

exemplo_moda <- c(1, 2, 2, 3, 4,4, 5, 6,7,7)
exemplo_moda
freq <- table(exemplo_moda)
freq

names(freq)[freq == max(freq)]

Moda <- function(x) {
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

Moda(exemplo_moda)

Moda(df$Marcelo)

Moda(participantes$sexo)



ggplot(participantes[participantes$idade, ], aes(x = idade, y = ..density..)) +
  
  geom_density(color = 'blue')


ggplot(participantes[participantes$idade, ], aes(x = idade, y = ..density..)) +
  geom_histogram(binwidth = 500) +
  geom_density(color = 'blue') +
  xlim(0,100)




moda <- as.numeric(Moda(participantes$idade))
moda

mediana <- median(participantes$idade)
mediana

media <- mean(participantes$idade)
media


print("Quartis, decis e percentis")

quantile(participantes$idade, c(0.25,0.5,0.75))

print("25% estao abaixo de 33 anos, 75% estao acima")
print("50% estao abaixo de 38.5 anos, 50% estao acima")
print("75% estao abaixo de 42 anos, 25% estao acima")
decis <- c()
for(i in 1:9){
  decis <- c(decis, i/10)
}
decis

quantile(participantes$idade, decis)


print("BoxPlot")

ggplot(data = participantes, aes(x= "", y = idade)) +
  stat_boxplot(geom='errorbar', width = 0.4) +
  geom_boxplot(fill = '#3274A1') +
  coord_flip() +
  ylab("anos") +
  xlab("") +
  ggtitle('Box-plot idades') +
  formatos


ggplot(data = participantes, aes(x=sexo, y=idade, group=sexo))+
  stat_boxplot(geom='errorbar', width = 0.4) +
  geom_boxplot(fill = c('#3274A1','orange','pink')) +
  coord_flip() +
  ylab("anos") +
  xlab("sexo") +
  ggtitle("box plot idade x sexo") +
  formatos


sexo <- c('Masculino', 'Feminino', 'Nao quis informar')

participantes$Cat.sexo <- factor(participantes$sexo)
levels(participantes$Cat.sexo) <- sexo


ggplot(data = participantes, aes(x=Cat.sexo, y=idade))+
  stat_boxplot(geom='errorbar', width = 0.4) +
  geom_boxplot(fill = c('#3274A1','orange','pink')) +
  coord_flip() +
  ylab("anos") +
  xlab("sexo") +
  ggtitle("box plot idade x sexo") +
  formatos



soma_linha <- function(linha) {
  for (item in linha) {
    numeric_item <- as.numeric(item)  # Convertendo para número
    cat("Item:", numeric_item)
    if (!is.na(numeric_item)) {  # Verifica se a conversão foi bem-sucedida
      if (numeric_item %% 2 == 0) {
        cat(" (par)")
      }
      cat("\n")
    } else {
      cat(" (não é um número)\n")
    }
  }
  cat("--------\n")
}

printa_linha <- function(linha) {
  for (item in linha) {
    
    cat("Item:", item)
    
  }
  cat("--------\n")
}
resultado <- apply(participantes[2, ], 1, printa_linha)


resultado <- apply(participantes[2, ], 1, soma_linha)


colunas_selecionadas <- c("Q1",	"Q2",	"Q3",	"Q4",	"Q5",	"Q6",	"Q7",	"Q8",	"Q9",	"Q10")
dados_selecionados <- participantes[, colunas_selecionadas]

dados_selecionados$Q1 <- dados_selecionados$Q1 - 1
dados_selecionados$Q2 <- abs(dados_selecionados$Q2 - 5)
dados_selecionados$Q3 <- dados_selecionados$Q3 - 1
dados_selecionados$Q4 <- abs(dados_selecionados$Q4 - 5)
dados_selecionados$Q5 <- dados_selecionados$Q5 - 1
dados_selecionados$Q6 <- abs(dados_selecionados$Q6 - 5)
dados_selecionados$Q7 <- dados_selecionados$Q7 - 1
dados_selecionados$Q8 <- abs(dados_selecionados$Q8 - 5)
dados_selecionados$Q9 <- dados_selecionados$Q9 - 1
dados_selecionados$Q10 <- abs(dados_selecionados$Q10 - 5)



table(dados_selecionados)

calcula_coeficiente_sus <- function(linha) {
  soma_dos_items <- 0
  
  for (item in linha) {
    numeric_item <- as.numeric(item)  # Convertendo para número
    if (numeric_item > 0) {
    
    if (!is.na(numeric_item)) {  # Verifica se a conversão foi bem-sucedida
      
      soma_dos_items <- soma_dos_items + numeric_item
      
    } 
      
    }
  }
  
  soma_dos_items <- soma_dos_items * 2.5
  
  return(soma_dos_items)
}

resultado <- apply(dados_selecionados[1, ], 1, calcula_coeficiente_sus)

num_linhas <- nrow(dados_selecionados)
resultados <- numeric(70)

for (i in 1:num_linhas) {
  linha_atual <- dados_selecionados[i, ]
  resultado <- calcula_coeficiente_sus(linha_atual)
  print(resultado)
  resultados[i] <- resultado
}


# Imprimir os resultados
cat("Resultados:", resultados, "\n")

for (i in nrow(resultados)) {
  cat(resultados[i])
}

participantes

# Medidas de dispersao

# Vamos começar vendo o desvio médio absoluto, que é - não deixa de ser uma média, 
# dos desvios das variáveis em relação a sua média.

notas_marcelo <- data.frame(Marcelo = df$Marcelo, row.names = row.names(df))
notas_marcelo

nota_media_marcelo <- mean(notas_marcelo$Marcelo)
notas_marcelo$Desvio <- notas_marcelo$Marcelo - nota_media_marcelo

notas_marcelo$Desvio.Absoluto <- abs(notas_marcelo$Marcelo - nota_media_marcelo)

notas_marcelo

mean(notas_marcelo$Desvio.Absoluto)



packageurl <- "https://cran.r-project.org/src/contrib/Archive/DescTools/DescTools_0.99.30.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

library(DescTools)


MeanAD(df$Marcelo)




materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química') 
Fulano <- c(8, 10, 4, 8, 6, 10, 8) 
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7) 

df <- data.frame(Fulano, Sicrano, row.names = materias) 

df 

MeanAD(df$Fulano)
MeanAD(df$Sicrano)


# Variancia
notas_marcelo$Desvio2 <- notas_marcelo$Desvio ^ 2

sum(notas_marcelo$Desvio2) / (nrow(notas_marcelo) - 1)

variancia <- var(notas_marcelo$Marcelo)
variancia


# Desvio padrao - Raiz quadrada da variancia
sqrt(variancia)

desvio_padrao <- sd(notas_marcelo$Marcelo)
desvio_padrao


 


notas_marcelo <- data.frame(Marcelo = df$Marcelo, row.names = row.names(df))
notas_marcelo

nota_media_marcelo <- mean(notas_marcelo$Marcelo)
notas_marcelo$Desvio <- notas_marcelo$Marcelo - nota_media_marcelo

notas_marcelo$Desvio.Absoluto <- abs(notas_marcelo$Marcelo - nota_media_marcelo)
notas_marcelo$Desvio.Padrao <-sd(notas_marcelo$Marcelo)
notas_marcelo

participantes



materias <- c("Matematica", "Portugues", "Ingles", "Geografia", "Historia", "Fisica", "Quimica")

questoes <- c("Q1",	"Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")

P1 <- c(5,1,3,2,3,0,3,3,4,2)
P2 <- c(5,1,4,1,5,1,4,1,4,2)
P3 <- c(5,1,5,1,5,1,5,1,5,1)
P4 <- c(4,1,4,1,3,2,5,1,4,1)
P5 <- c(5,3,3,3,3,3,3,3,3,3)
P6 <- c(2,3,3,1,4,0,5,2,2,4)
P7 <- c(5,1,5,1,5,1,5,1,5,1)
P8 <- c(5,2,4,2,4,1,3,1,4,2)
P9 <- c(5,1,4,1,5,1,3,1,4,1)
P10 <- c(5,1,5,4,5,1,3,1,5,2)
P11 <- c(2,3,3,1,2,3,1,2,4,1)
P12 <- c(4,2,3,3,3,2,2,2,3,4)
P13 <- c(3,2,3,1,4,1,3,1,2,4)
P14 <- c(4,1,4,1,5,1,5,1,4,1)
P15 <- c(1,1,5,1,3,1,5,1,5,1)
P16 <- c(3,3,2,1,3,3,3,3,3,1)
P17 <- c(4,1,3,1,4,1,4,1,3,1)
P18 <- c(5,1,3,2,5,1,3,3,3,1)
P19 <- c(5,1,5,1,5,1,5,1,5,1)
P20 <- c(5,1,5,1,5,1,5,1,5,1)
P21 <- c(4,4,5,3,5,1,5,1,5,1)
P22 <- c(5,2,4,1,5,3,5,1,4,1)
P23 <- c(5,1,4,1,4,2,5,1,5,1)
P24 <- c(4,1,3,3,4,1,4,2,5,1)
P25 <- c(4,2,3,4,3,2,2,4,3,4)
P26 <- c(3,1,3,1,3,1,4,1,2,1)
P27 <- c(5,1,5,2,5,1,5,1,4,3)
P28 <- c(5,1,5,1,5,1,5,5,5,5)
P29 <- c(5,5,5,2,5,1,5,1,4,4)
P30 <- c(4,3,2,1,2,5,4,2,3,2)
P31 <- c(5,1,5,1,4,1,4,1,5,1)
P32 <- c(2,2,3,1,1,4,3,2,5,1)
P33 <- c(5,5,4,4,4,4,5,5,5,5)
P34 <- c(0,3,3,3,3,3,3,3,2,4)
P35 <- c(4,1,5,1,5,1,5,1,5,1)
P36 <- c(3,3,2,1,3,3,3,3,3,3)
P37 <- c(5,1,3,1,5,1,5,1,5,1)
P38 <- c(4,1,5,1,4,1,5,1,5,1)
P39 <- c(5,1,5,4,4,1,4,4,4,3)
P40 <- c(3,3,3,3,4,2,2,2,4,1)
P41 <- c(4,3,3,3,4,0,4,3,3,3)
P42 <- c(3,3,3,1,3,2,3,3,3,3)
P43 <- c(5,1,5,1,5,1,5,1,5,2)
P44 <- c(5,4,3,2,5,3,2,3,2,4)
P45 <- c(4,1,3,1,5,0,4,3,3,1)
P46 <- c(4,1,5,1,5,1,5,1,4,1)
P47 <- c(4,2,4,2,4,3,4,2,4,5)
P48 <- c(4,1,5,1,5,1,5,1,5,1)
P49 <- c(4,1,5,1,4,1,5,1,5,1)
P50 <- c(0,1,4,1,4,1,0,1,0,1)
P51 <- c(1,3,1,4,1,0,2,4,1,3)
P52 <- c(4,1,5,1,3,4,5,1,5,1)
P53 <- c(4,2,3,4,4,2,4,2,0,1)
P54 <- c(3,4,1,1,3,1,3,3,3,1)
P55 <- c(3,1,3,3,5,1,5,1,3,3)
P56 <- c(5,0,5,1,0,0,0,1,4,1)
P57 <- c(3,3,4,2,4,2,3,2,4,2)
P58 <- c(5,1,5,5,1,1,5,1,5,1)
P59 <- c(4,1,4,1,4,2,5,1,5,1)
P60 <- c(4,1,4,1,4,1,5,2,4,2)
P61 <- c(3,1,5,1,3,0,5,1,5,1)
P62 <- c(3,0,4,1,5,1,4,1,4,1)
P63 <- c(5,1,4,2,4,1,5,1,4,2)
P64 <- c(5,2,4,1,5,1,5,1,4,1)
P65 <- c(2,3,5,2,5,2,4,2,3,1)
P66 <- c(3,1,4,1,3,1,4,4,4,3)
P67 <- c(4,2,4,1,4,2,4,1,4,1)
P68 <- c(5,1,4,1,5,1,4,1,5,1)
P69 <- c(5,1,3,3,5,1,3,5,5,2)
P70 <- c(4,2,4,1,3,1,4,4,3,1)


dfmestrado <- data.frame(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,
                 P21,P22,P23,P24,P25,P26,P27,P28,P29,P30,P31,P32,P33,P34,P35,P36,P37,P38,
                 P39,P40,P41,P42,P43,P44,P45,P46,P47,P48,P49,P50,P51,P52,P53,P54,P55,P56,
                 P57,P58,P59,P60,P61,P62,P63,P64,P65,P66,P67,P68,P69,P70, row.names = questoes)


dfmestrado$P1


sd(dfmestrado$P1)

participantes_de_23_a_29 <- c("P2","P8","P9","P22","P23","P29","P30","P32","P33","P39","P48","P63","P67")
de_23_a_29 <- c(90,80,90,87.5,92.5,77.5,55,60,50,72.5,97.5,87.5,82.5)
              
dfsus_de_23_a_29 <- data.frame(de_23_a_29, row.names = participantes_de_23_a_29)
dfsus_de_23_a_29


mean(dfsus_de_23_a_29$de_23_a_29)
sd(dfsus_de_23_a_29$de_23_a_29)


participantes_de_30_a_39 <-c('P1','P4','P6','P10','P11','P13','P15','P17','P18','P25','P26','P27','P35','P36','P38','P41','P45','P46','P47','P52','P53','P57','P58','P61','P64','P65')
de_30_a_39 <- c(62.5,85,52.5,85,55,65,85,82.5,77.5,47.5,75,90,97.5,52.5,95,52.5,70,95,65,85,62.5,67.5,80,80,92.5,72.5)
dfsus_de_30_a_39 <- data.frame(de_30_a_39, row.names = participantes_de_30_a_39)
mean(dfsus_de_30_a_39$de_30_a_39)
sd(dfsus_de_30_a_39$de_30_a_39)

participantes_de_40_a_49 <- c('P7','P16','P19','P20','P24','P28','P34','P37','P40','P42','P43','P44','P49','P51','P54','P55','P56','P60','P68','P69','P70')
de_40_a_49 <- c(100,57.5,100,100,80,80,40,95,62.5,57.5,97.5,52.5,95,17.5,57.5,75,57.5,85,95,72.5,72.5)
dfsus_de_40_a_49 <- data.frame(de_40_a_49, row.names = participantes_de_40_a_49)

mean(dfsus_de_40_a_49$de_40_a_49)
sd(dfsus_de_40_a_49$de_40_a_49)

participantes_de_50_a_59 <- c('P3','P14','P21','P31','P50','P66')
de_50_a_59 <- c(100,92.5,85,95,65,70)
dfsus_de_50_a_59 <- data.frame(de_50_a_59, row.names = participantes_de_50_a_59)

mean(dfsus_de_50_a_59$de_50_a_59)
sd(dfsus_de_50_a_59$de_50_a_59)

participantes_de_60_a_67 <- c('P5','P12','P59','P62')
de_60_a_67 <- c(55,55,90,77.5)
dfsus_de_60_a_67 <- data.frame(de_60_a_67, row.names = participantes_de_60_a_67)

mean(dfsus_de_60_a_67$de_60_a_67)
sd(dfsus_de_60_a_67$de_60_a_67)


participantes_homens <- c('P7','P10','P11','P12','P13','P24','P28','P29','P30','P31','P32','P34','P35','P42','P50','P57','P58','P64','P65','P66','P67','P68','P69','P70')
homens <- c(100,85,55,55,65,80,80,77.5,55,95,60,40,97.5,57.5,65,67.5,80,92.5,72.5,70,82.5,95,72.5,72.5)
dfparticipantes_homens <- data.frame(homens, row.names = participantes_homens)
mean(dfparticipantes_homens$homens)
sd(dfparticipantes_homens$homens)

participantes_mulheres <- c('P1','P2','P3','P4','P5','P6','P8','P9','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P25','P26','P27','P33','P36','P37','P39','P40','P41','P43','P44','P45','P46','P47','P48','P49','P51','P53','P54','P55','P56','P59','P60','P61','P62','P63')
mulheres <- c(62.5,90,100,85,55,52.5,80,90,92.5,85,57.5,82.5,77.5,100,100,85,87.5,92.5,47.5,75,90,50,52.5,95,72.5,62.5,52.5,97.5,52.5,70,95,65,97.5,95,17.5,62.5,57.5,75,57.5,90,85,80,77.5,87.5)
dfparticipantes_mulheres <- data.frame(mulheres, row.names = participantes_mulheres)

mean(dfparticipantes_mulheres$mulheres)
sd(dfparticipantes_mulheres$mulheres)


participantes_nao_quis_informar <- c('P38', 'P52')
nao_quis_informar <- c(95, 85)
dfnao_quis_informar <- data.frame(nao_quis_informar, row.names = participantes_nao_quis_informar)
mean(dfnao_quis_informar$nao_quis_informar)
sd(dfnao_quis_informar$nao_quis_informar)

round(78.65385)
round(74.23077)
round(73.80952)
84.58333
69.375












