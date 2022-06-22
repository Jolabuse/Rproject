opposeLine <- function(tbl){#if you have a greater than you take the opposite of the line
  for(ind in c(1:nrow(tbl))){
    if(tbl[ind,(ncol(tbl)-1)] == 1){
      tbl[ind,] <- -1 * tbl[ind,]
    }
  }
  return(tbl)
}

leafConstructionZ <- function(tbl,varTbl){
  z<-0
  for(ind in c(1:length(varTbl))){
    if(varTbl[ind] == 1){
      z<- z + tbl[1,ind]
    }
    else{
      if(varTbl[ind] != 0 && tbl[1,ind]>0){
        z<- z + tbl[1,ind]
      }
    }
  }
  return(z)
}

leafConstructionTeta <- function(tbl,varTbl){
  tetaTbl <- matrix(nrow = 1,ncol = (nrow(tbl)-1))
  for (ind in c(1:(nrow(tbl)-1))) {
    tetaTbl[ind]<-tbl[(ind+1),ncol(tbl)]
  }
  for(ind1 in c(1:(nrow(tbl)-1))){
    for(ind in c(1:length(varTbl))){
      if(varTbl[ind] == 1){
        tetaTbl[ind1]<-tetaTbl[ind1]-tbl[(1+ind1),ind]
      }
      else{
        if(varTbl[ind] != 0 && tbl[ind1+1,ind]<0){
          tetaTbl[ind1]<-tetaTbl[ind1]-tbl[(1+ind1),ind]
        }
      }
    }
  }
  row.names(tetaTbl)="teta"
  return(tetaTbl)
}


createTreeTable <- function(tbl){
  treeTable <- matrix(ncol = (1+nrow(tbl)-1+ncol(tbl)-2))
  for(ind in c(1:(ncol(tbl)-1))){
    treeTable[ind]<- -1
  }
  for(ind in c((ncol(tbl)-1):(ncol(tbl)))){
    treeTable[ind]<- leafConstructionZ(tbl,treeTable[1:(ncol(tbl)-2)])
  }
  tetaTbl <- leafConstructionTeta(tbl,treeTable[1:(ncol(tbl)-2)])
  for(ind in c((ncol(tbl)):(ncol(tbl)-2 + nrow(tbl)))){
    treeTable[ind]<- tetaTbl[ind-(ncol(tbl)-1)]
  }
  return(treeTable)
}

newLeaf <- function(tbl,treeTable,ind1,ind2,value){
  leaf <- matrix(ncol = (1+nrow(tbl)-1+ncol(tbl)-2))
  for(ind in c(1:(ind2-1))){
    leaf[ind] <- treeTable[ind1,ind]
  }
  leaf[ind2] <- value
  if(ind2<ncol(tbl)-2){
    for(ind in c((ind2+1):(ncol(tbl)-2))){
      leaf[ind]<- -1
    }
  }
  leaf[(ncol(tbl)-1)]<- leafConstructionZ(tbl,leaf[1:(ncol(tbl)-2)])
  tetaTbl <- leafConstructionTeta(tbl,leaf[1:(ncol(tbl)-2)])
  for(ind in c((ncol(tbl)):(ncol(tbl)-2 + nrow(tbl)))){
    leaf[ind]<- tetaTbl[ind-(ncol(tbl)-1)]
  }
  return(leaf)
  
}

divideInTwoLeaves <- function(tbl,treeTable,ind){
  ind2<-1
  while(treeTable[ind,ind2]!=-1){
    ind2<-ind2+1;
  }
  line1 <- newLeaf(tbl,treeTable,ind,ind2,1)
  line0 <- newLeaf(tbl,treeTable,ind,ind2,0)
  if(nrow(treeTable)>1){
    treeTable <- rbind(line1,line0,treeTable[((ind+1):nrow(treeTable)),])
  }else{
    treeTable <- rbind(line1,line0)
  }
  return(treeTable)
  
}

checkTeta <- function(treeNodeTeta){
  for (variable in treeNodeTeta) {
    if(variable <0){
      return(FALSE)
    }
  }
  return(TRUE)
}


checkMaxZ <- function(maxNode,treeNode,nvar){
  if(!all(is.na(maxNode))){
    print("it's me")
    if(maxNode[nvar+1]<treeNode[nvar+1]){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(TRUE)
}

pop <-function(treeTable){
  
  if(nrow(treeTable)>1){
    if(nrow(treeTable)==2){
      treeTable <- matrix(nrow=1,ncol=ncol(treeTable),treeTable[2,])
    }else{
      treeTable <- treeTable[(2:nrow(treeTable)),]
    }
  }else{
    treeTable <- NA
  }
  return(treeTable)
}

checkVarAssignement<-function(treeNode,nvar){
  for(ind in c(1:nvar)){
    if(treeNode[ind] == -1){
      return(TRUE)
    }
  }
  return(FALSE)
}


tbl <- read.delim(file.choose(), header = FALSE, sep = " ") # store in a table the input file
tbl <- opposeLine(tbl)
print(tbl)
treeTable <- createTreeTable(tbl)
maxNode <- NA
nvar <- ncol(tbl)-2
nconst <- nrow(tbl)-1
print(treeTable)
while (!all(is.na(treeTable))) {
  if(checkTeta(treeTable[1,((nvar+2):(nvar+1+nconst))])){
    if(checkMaxZ(maxNode,treeTable[1,],nvar)){
      if(checkVarAssignement(treeTable[1,],nvar)){
        treeTable<-divideInTwoLeaves(tbl,treeTable,1)
      }else{
        print("max")
        maxNode <- treeTable[1,]
        treeTable <- pop(treeTable)
      }
      
    }else{
      print("inferior to z")
      treeTable<-pop(treeTable)
    }
  }else{
    print("teta")
    treeTable<-pop(treeTable)
  }
  print(treeTable)
}

print("Max is :")
print(maxNode)