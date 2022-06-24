opposeLine <- function(tbl){#if you have a greater than you take the opposite of the line
  for(ind in c(1:nrow(tbl))){
    if(tbl[ind,(ncol(tbl)-1)] == 1){
      tbl[ind,] <- -1 * tbl[ind,]
    }
  }
  return(tbl)
}

#calculate Z with the variables values
leafConstructionZ <- function(tbl,varTbl){
  z<-0
  for(ind in c(1:length(varTbl))){
    if(varTbl[ind] == 1){                           #if 1 we add
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

#calculate the teta for each constraints
leafConstructionTeta <- function(tbl,varTbl){
  tetaTbl <- matrix(nrow = 1,ncol = (nrow(tbl)-1))
  for (ind in c(1:(nrow(tbl)-1))) {
    tetaTbl[ind]<-tbl[(ind+1),ncol(tbl)] #
  }
  for(ind1 in c(1:(nrow(tbl)-1))){
    for(ind in c(1:length(varTbl))){
      if(varTbl[ind] == 1){                               #verify if variable is 1 then add to the total teta value
        tetaTbl[ind1]<-tetaTbl[ind1]-tbl[(1+ind1),ind]    
      }
      else{
        if(varTbl[ind] != 0 && tbl[ind1+1,ind]<0){        #verify if the variable is less than 0 
          tetaTbl[ind1]<-tetaTbl[ind1]-tbl[(1+ind1),ind]  #and not assigned then to the total add teta value
        }
      }
    }
  }
  row.names(tetaTbl)="teta"
  return(tetaTbl)
}

#creating the tree of the table
createTreeTable <- function(tbl){
  treeTable <- matrix(ncol = (1+nrow(tbl)-1+ncol(tbl)-2))
  for(ind in c(1:(ncol(tbl)-1))){ 
    treeTable[ind]<- -1 
  }
  for(ind in c((ncol(tbl)-1):(ncol(tbl)))){               #insert value of Z in the tree
    treeTable[ind]<- leafConstructionZ(tbl,treeTable[1:(ncol(tbl)-2)])
  }
  tetaTbl <- leafConstructionTeta(tbl,treeTable[1:(ncol(tbl)-2)])
  for(ind in c((ncol(tbl)):(ncol(tbl)-2 + nrow(tbl)))){   #insert teta in the tree
    treeTable[ind]<- tetaTbl[ind-(ncol(tbl)-1)]
  }
  return(treeTable)
}

#creating the new leaf of variable
newLeaf <- function(tbl,treeTable,ind1,ind2,value){       
  leaf <- matrix(ncol = (1+nrow(tbl)-1+ncol(tbl)-2))
  for(ind in c(1:(ind2-1))){
    leaf[ind] <- treeTable[ind1,ind]                      #copy the ancien values of variables in the new leaf
  }
  leaf[ind2] <- value                                     #put 1 or 0 to the first non assigned variable
  if(ind2<ncol(tbl)-2){
    for(ind in c((ind2+1):(ncol(tbl)-2))){                
      leaf[ind]<- -1                                      #complete the non assigned variable 
    }
  }
  leaf[(ncol(tbl)-1)]<- leafConstructionZ(tbl,leaf[1:(ncol(tbl)-2)])    #insert the new value of Z
  tetaTbl <- leafConstructionTeta(tbl,leaf[1:(ncol(tbl)-2)])            #insert new value of teta
  for(ind in c((ncol(tbl)):(ncol(tbl)-2 + nrow(tbl)))){                 
    leaf[ind]<- tetaTbl[ind-(ncol(tbl)-1)]
  }
  return(leaf)
  
}

#creating two new nodes and deleting the old one and replacing by the new ones
divideInTwoLeaves <- function(tbl,treeTable,ind){
  ind2<-1
  while(treeTable[ind,ind2]!=-1){
    ind2<-ind2+1;
  }
  line1 <- newLeaf(tbl,treeTable,ind,ind2,1)      #create new leaf with a 1
  line0 <- newLeaf(tbl,treeTable,ind,ind2,0)      #create new leaf with a 0
  if(nrow(treeTable)>1){                                  
    treeTable <- rbind(line1,line0,treeTable[((ind+1):nrow(treeTable)),])  #replace the old leaf by the two new ones
  }else{
    treeTable <- rbind(line1,line0)
  }
  return(treeTable)
  
}

#verify if teta is < 0
checkTeta <- function(treeNodeTeta){
  for (variable in treeNodeTeta) {
    if(variable <0){
      return(FALSE)
    }
  }
  return(TRUE)
}

#check if Z < Z*
checkMaxZ <- function(maxNode,treeNode,nvar){
  if(!all(is.na(maxNode))){
    if(maxNode[nvar+1]<treeNode[nvar+1]){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(TRUE)
}

#remove first element of tree table
pop <-function(treeTable){
  
  if(nrow(treeTable)>1){
    if(nrow(treeTable)==2){
      treeTable <- matrix(nrow=1,ncol=ncol(treeTable),treeTable[2,])
    }else{
      treeTable <- treeTable[(2:nrow(treeTable)),]         
    }
  }else{
    treeTable <- NA                                       #replacing by an empty tree 
  }
  return(treeTable)
}

checkVarAssignement<-function(treeNode,nvar){  #check if all variables have a value or not
  for(ind in c(1:nvar)){
    if(treeNode[ind] == -1){
      return(TRUE)
    }
  }
  return(FALSE)
}

binaryTree <-function(tbl){ #main function
  print(tbl)
  treeTable <- createTreeTable(tbl) #we create the tree table
  maxNode <- NA                     
  nvar <- ncol(tbl)-2
  nconst <- nrow(tbl)-1
  print(treeTable)
  
  #print the result of the route of the tree
  while (!all(is.na(treeTable))) {  #we stop when our tree is empty
    if(checkTeta(treeTable[1,((nvar+2):(nvar+1+nconst))])){ #if tetas > 0
      if(checkMaxZ(maxNode,treeTable[1,],nvar)){            #if Z < Z*
        if(checkVarAssignement(treeTable[1,],nvar)){        #if all variables are not assigned
          print("We divide and erase the first leaf into two leaves")
          treeTable<-divideInTwoLeaves(tbl,treeTable,1)     #We divide and erase the first leaf into two leaves
        }else{
          print("We define the first Z*")                   #we define the first Z* and delete it from the tree
          maxNode <- treeTable[1,]
          treeTable <- pop(treeTable)
        }
        
      }else{
        print("This table is inferior to z*, we don't use it")   #we doesn't need to develop this branch because Z<Z*
        treeTable<-pop(treeTable)                                #so we delete it
      }
    }else{
      print("At least 1 teta is less than 0")       #we doesn't need to develop this branch because at least one teta <0
      treeTable<-pop(treeTable)                     #so we delete it
    }
    print(treeTable)
  }
  
  print("Max is :")
  print(maxNode[nvar+1])
  print("accessed with values :")
  print(maxNode[1:nvar])
}

tbl <- read.delim(file.choose(), header = FALSE, sep = " ") # store in a table the input file 
tbl <- opposeLine(tbl)
binaryTree(tbl)



