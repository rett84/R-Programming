best <- function(state, outcome) {
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
  
  
  
  #cria data frame a partir de um subset com o criterio do estado(state) desejado.
  subset_estado<-subset(dados, dados[,7]== state)
  
  
  ## Return hospital name in that state with lowest 30-day death
  
  #DATASET$NAME[DATASET$COLUMNNAME == min(DATASET$COLUMNNAME)]
  #subset_estado[subset_estado[,17] == min(subset_estado[,17]) ,"Hospital.Name"]
  #subset_estado[,2][subset_estado[,17] == min(subset_estado[,17])]
  subset_estado[which.min(subset_estado[,outcome_col]),"Hospital.Name"]
  

}