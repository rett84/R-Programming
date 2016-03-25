rankhospital <- function(state, outcome, num = 1) {
  ## Read outcome data
  
  dados <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  #checa estado
  estado_existe<-0
  
  for (i in 1:nrow(dados) ){
    
    if (dados[i,7] == state) {
      
      estado_existe=estado_existe+1
      
    }
  }
  
  if (estado_existe==0) {
    
    stop("estado n existe")
  }
  
  #checa outcome
  if (outcome == "heart attack") {
    
    outcome_col<-11
    #as  colunas com os dados numericos devem ser numericas
    dados[,11] <- as.numeric(dados[,11])
  } 
  else if (outcome == "heart failure") {
    
    outcome_col<-17
    #as  colunas com os dados numericos devem ser numericas
    dados[,17] <- as.numeric(dados[,17])
  }
  
  else if (outcome == "pneumonia") {
    
    outcome_col<-23
    #as  colunas com os dados numericos devem ser numericas
    dados[,23] <- as.numeric(dados[,23])
    
  }
  
  else {
    
    stop("outcome nao valido")
    
  }
  
  
 
  
  
  
  ## Return hospital name in that state with the given rank
  
  #cria data frame a partir de um subset com o criterio do estado(state) desejado.
  subset_estado<-subset(dados, dados[,7]== state & dados[,outcome_col]!= "Not Available" )
  
  
  #dd[ order(-dd[,4], dd[,1]), ]
  
  
  #ordena o data frame pelos criterios heart attack, heart failure ou pnemonia, 
  #e depois por ordem alfabertica
  subset_order<-subset_estado[order(subset_estado[,outcome_col], subset_estado[,2] ), ]

  

  #criterio "best ou worst
  if (num=="best") {
    
    num<-1
  }
  else if (num=="worst") {
    
    num<-nrow(subset_order)
    
  }
  
 #seleciona o item desejado  
 subset_order[num,2]
 

}
