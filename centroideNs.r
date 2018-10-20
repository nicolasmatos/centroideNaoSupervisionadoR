calculaCentroides <- function(dataset, n) {
  somatorios <- list()
  
  for (i in 1: n) {
    somatorios[[i]] = 0
    somatorios[[i]][1] = 0
    somatorios[[i]][2] = 0
    somatorios[[i]][3] = 0
    somatorios[[i]][4] = 0
    somatorios[[i]][5] = 0
  }
  
  for (i in 1: nrow(dataset)) {
    for (j in 1: n) {
      if (dataset[i,6] == j) {
        somatorios[[j]][1] = somatorios[[j]][1] + dataset[i,1]
        somatorios[[j]][2] = somatorios[[j]][2] + dataset[i,2]
        somatorios[[j]][3] = somatorios[[j]][3] + dataset[i,3]
        somatorios[[j]][4] = somatorios[[j]][4] + dataset[i,4]
        somatorios[[j]][5] = somatorios[[j]][5] + 1
      }
    }
  }
  
  centroides<-list()
  for (i in 1: n) {
    centroides[[i]] = c((somatorios[[i]][1] / somatorios[[i]][5]), (somatorios[[i]][2] / somatorios[[i]][5]), (somatorios[[i]][3] / somatorios[[i]][5]), (somatorios[[i]][4] / somatorios[[i]][5]))
  }
  
  return (centroides)
}

classificaLinha <- function(centroides, n, e) {
  distancias<-c()
  for (i in 1: n) {
    distancias[i] = sqrt((centroides[[i]][1] - e[1])^2 + (centroides[[i]][2] - e[2])^2 + (centroides[[i]][3] - e[3])^2 + (centroides[[i]][4] - e[4])^2)
  }
  
  menor = min(distancias)
  indiceMenorDistancia = match(menor,distancias)
  
  return(indiceMenorDistancia)
}

processaCentroideNs <- function(dataset, n) {
  centroidesIniciais <- list()
  random <- c()
  
  #Gera n numeros aleatórios para serem os centroides inicias
  random = (sample(1:nrow(dataset), n, replace=F))
  for (i in 1:n) {
    centroidesIniciais[[i]] = dataset[random[i],]
    centroidesIniciais[[i]][6] = i
  }
  
  #Laço para percorrer todas as linha do dataframe para fazer a classificação inicial com base nos centroides aleatórios
  for (i in 1: (nrow(dataset))) {
    #Recebe a linha atual do conjunto
    linha = dataset[i,]
    
    #Faz a classificação do dataset com base nos centorides iniciais
    dataset[i,6] = classificaLinha(centroidesIniciais, n, linha)
  }
  
  centroidesAtuais <- list()
  centroidesNovos <- list()
  #Calcula os centroides depois da classificação inicial
  centroidesAtuais = calculaCentroides(dataset, n)
  
  ctrl = TRUE
  numeroInteracoes = 0
  #Laço que irá se repetir até os centroidesNovos e os atuais forem iguais
  while(ctrl) {
    #Laço para percorrer todas as linha do dataframe para fazer a classificação com base nos centroides atuais
    for (i in 1: (nrow(dataset))) {
      #Recebe a linha atual do conjunto
      linha = dataset[i,]
      #Faz a classificação do dataset com base nos centorides
      dataset[i,6] = classificaLinha(centroidesAtuais, n, linha)
    }
    
    #Calcula os novos centroides após a classificação do dataset
    centroidesNovos = calculaCentroides(dataset, n)
    
    #r = list()
    #r$novo = centroidesNovos
    #r$atual = centroidesAtuais
    
    #Verificando se os centroides são iguais
    todoMundoIgual = TRUE
    for (i in 1: n) {
      if(centroidesNovos[[i]] != centroidesAtuais[[i]]) {
        todoMundoIgual = FALSE
      }
    }
    
    #Se todos os centroides forem iguais para o processamento
    if (todoMundoIgual) {
      ctrl = FALSE
    }
    else {
      centroidesAtuais = centroidesNovos
    }
    
    #Contando quantas interações houveram até o centroide não mudar mais
    numeroInteracoes = numeroInteracoes + 1
  }
  
  #Retorno
  r = list()
  r$centroidesIniciais = centroidesIniciais
  r$centroidesFinais = centroidesNovos
  r$dataset = dataset
  r$numeroInteracoes = numeroInteracoes
  options(warn=-1)
  
  return(r)
}