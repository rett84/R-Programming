rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  dados <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #ordena o data frame pelos criterios de ordem alfabetica do estado 
  dados_order<-dados[order(dados[,7]  ), ]
  
  #Cria data frame vazio para ser populado posteriormente
  df <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE) 
  
  #pega nomes unicos de cada estado
  estados<-c(unique(dados[,7]))
  
  #organiza o vetor por ordem alfabetica
  estados_sort<-sort(estados)
  
  
  #checa outcome
  if (outcome == "heart attack") {
    
    outcome_col<-11
    #as  colunas com os dados numericos devem ser numericas
    dados_order[,11] <- as.numeric(dados_order[,11])
  } 
  else if (outcome == "heart failure") {
    
    outcome_col<-17
    #as  colunas com os dados numericos devem ser numericas
    dados_order[,17] <- as.numeric(dados_order[,17])
  }
  
  else if (outcome == "pneumonia") {
    
    outcome_col<-23
    #as  colunas com os dados numericos devem ser numericas
    dados_order[,23] <- as.numeric(dados_order[,23])
    
  }
  
  else {
    
    stop("outcome nao valido")
    
  }
  
  
  ## For each state, find the hospital of the given rank
  
 
  
  #faz o loop para cada estado, 
  for (i in 1:length(estados)) {

   #cria data frame a partir de um subset com o criterio do estado que o loop for
   #esta executando.
   subset_estado<-subset(dados_order, dados_order[,7]==estados_sort[i] & 
                           dados_order[,outcome_col]!= "Not Available")
   
 
   #ordena o data frame pelos criterios heart attack, heart failure ou pnemonia, 
   #e depois por ordem alfabertica
   subset_order<-subset_estado[order(subset_estado[,outcome_col], subset_estado[,2]  ), ]


   
   #criterio "best ou worst
   if (num=="best") {
    
     num1<-1
   }
   else if (num=="worst") {
     
     num1<-nrow(subset_order)
     
   }
   else {
     num1<-num
   }

   
   
  #popula cada linha do data frame  com os criterios do estado e do ranking desejado
  df[i, ] <- c( subset_order[num1,2], estados_sort[i])
   
  
  }
  
 #mostra data frame
 df
#test<<-subset_order
#nrow(test)
}