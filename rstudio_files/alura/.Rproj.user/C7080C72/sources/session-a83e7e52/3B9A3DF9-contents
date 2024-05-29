participantes <- read.csv("/home/rstudio/participantes.csv")

colunas_selecionadas <- c("Q1",	"Q2",	"Q3",	"Q4",	"Q5",	"Q6",	"Q7",	"Q8",	"Q9",	"Q10")
dados_selecionados <- participantes[, colunas_selecionadas]

calcula_coeficiente_sus <- function(linha) {
  soma_dos_items <- 0
  questao = 0
  
  for (item in linha) {
    questao = questao + 1
    
    numeric_item <- as.numeric(item)  # Convertendo para número
    if (numeric_item > 0) {
      
      if (!is.na(numeric_item)) {  # Verifica se a conversão foi bem-sucedida
        
        if (questao %% 2 == 0) {
          numeric_item = abs(numeric_item - 5)
          
        } else {
          numeric_item = numeric_item - 1
        }
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











