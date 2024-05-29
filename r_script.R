options(repos = "http://cran-r.c3sl.ufpr.br/")
# sessionInfo()
# dados <- read.csv("dados.csv")
participantes <- read.csv("participantes.csv")

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
plot.new()