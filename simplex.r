tbl = read.delim(file.choose(),header = FALSE,sep=' ') #store in a table the input file

nbNonBase <- ncol(tbl)-2

addBaseCol <- function(tbl,ind){ #insert a base variable for the ind row at the end of A
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1]<-1
  tbl<-cbind(tbl[,1:(ncol(tbl)-2)],bCol,tbl[,(ncol(tbl)-1):ncol(tbl)])
}

addNegBaseCol <- function(tbl,ind){ #insert a base variable for the ind row at the end of A
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1]<- -1
  tbl<-cbind(tbl[,1:(ncol(tbl)-2)],bCol,tbl[,(ncol(tbl)-1):ncol(tbl)])
}

addInvBaseCol <- function(tbl,ind){ #invert the values of the ind row and insert a base variable for it at the end of A
  tbl[ind,1:(ncol(tbl)-2)]<- tbl[ind,1:(ncol(tbl)-2)]*-1
  tbl[ind,ncol(tbl)]<- tbl[ind,ncol(tbl)]*-1
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1] <- 1
  tbl<-cbind(tbl[,1:(ncol(tbl)-2)],bCol,tbl[,(ncol(tbl)-1):ncol(tbl)])
}


for (ind in c(1:nrow(tbl))) { #create the table
  if(tbl[ind,(ncol(tbl)-1)] == -1){
      tbl <- addBaseCol(tbl,ind)
  }
  if(tbl[ind,(ncol(tbl)-1)] == 1){
    if(tbl[ind,(ncol(tbl))] < 0 ){
         tbl <- addInvBaseCol(tbl,ind)
    }
    else{
      tbl <- addNegBaseCol(tbl,ind)
    }
  }
}

tbl<- cbind(tbl[,1:(ncol(tbl)-2)],tbl[,(ncol(tbl))])#erase the inequality column
tbl[nrow(tbl),ncol(tbl)]<-0 #put the 0 for the Z-0
tbl

posBaseDetection <- function(tbl){
  bases <- vector(length = nrow(tbl)-1)
  sum1 <-colSums(tbl[,1:(ncol(tbl)-1)])
  sum1 <- which(sum1 == 1)
  for (ind in sum1) {
    detBas <- which(tbl[,ind] == 1)
    if(length(detBas) == 1){
      bases[detBas] <- ind
    }
  }
  return(bases)
}

nonBaseDetection <- function(tbl){
  bases <- vector(length = 0)
  sum1 <- colSums(tbl[,1:(ncol(tbl)-1)])
  sum2 <- which(sum1 != 1)
  sum1 <- which(sum1 == 1)
  for(ind in sum2){
    bases <- append(bases,ind)
  }
  for (ind in sum1) {
    detBas <- which(tbl[,ind] != 1 & tbl[,ind] != 0)
    if(length(detBas != 0)){
      bases <- append(bases,ind)
    }
  }
  return(bases)
}


allBaseDetection <- function(tbl){
  bases <- vector(length = nrow(tbl)-1)
  sum1 <-colSums(tbl[,1:(ncol(tbl)-1)])
  sum1 <- which(sum1 == 1 |sum1 == -1)
  for (ind in sum1) {
    detBas <- which(tbl[,ind] == 1 | tbl[,ind] == -1)
    if(length(detBas) == 1){
      bases[detBas] <- ind
    }
  }
  return(bases)
}

allBasesInd <- allBaseDetection(tbl) #find the base variables


addArtVar <- function(tbl,ind){#add artificial variable for an index
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1]<-1
  tbl<-cbind(tbl[,1:(ncol(tbl)-1)],bCol,tbl[,ncol(tbl)])
  tbl[nrow(tbl),ncol(tbl)-1] <- -1
  return(tbl)
}

doubleSimplex <- 0
cptNegBase <- 0
print(tbl)

for(ind in (1:length(allBasesInd))){# add all artificial variables and set up C
  if(tbl[ind,allBasesInd[ind]]==-1){
    if(doubleSimplex == 0){
      C <- tbl[nrow(tbl),]
      tbl[nrow(tbl),] <- 0
    }
    doubleSimplex <- 1
    cptNegBase <- cptNegBase+1
    tbl<-addArtVar(tbl,ind)
    tbl[nrow(tbl),] <- tbl[nrow(tbl),] + tbl[ind,]
  }
}

print(tbl)

renameRows <- function(tbl){ #rename the rows
  names=c()
  posBases <- posBaseDetection(tbl)
  for (val in posBases) {
    names<-append(names,paste("x",val,sep=""))
  }
  names <-append(names,"C")
  rownames(tbl)<-names
  return(tbl)
}


names=c()  #rename the col & rows
for (ind in c(1:(ncol(tbl)-1))) {
  names<-append(names,paste("x",ind,sep=""))
}
names <-append(names,"B")
colnames(tbl)<-names
tbl<-renameRows(tbl)

print(tbl)



continueCondition <- function(tbl){ #check if only negative in C
  C <- tbl[nrow(tbl),1:(ncol(tbl)-1)]
  for (variable in C) {
    if(variable>0){
      return(0)
    }
  }
  return(1)
}

selection <- function(tbl){ #we select the biggest in C and the min positive in B/C in the selected column
  C <- tbl[nrow(tbl),1:(ncol(tbl)-1)]
  B <- tbl[1:(nrow(tbl)-1),ncol(tbl)]
  maxC <- max(C)
  maxC <- head(which(C == maxC),1)
  cMaxC <- tbl[1:(nrow(tbl)-1),maxC]
  BcMaxC <- B/cMaxC
  minB <- head(which(BcMaxC == min(BcMaxC[BcMaxC>0])),1)
  c(minB,maxC)
}

gauss_pivot<-function(mat,pivotLine,pivotColumn){
  n = mat[pivotLine,pivotColumn]
  mat[pivotLine,] = mat[pivotLine,]/n
  for(i in 1:nrow(mat)){
    if(pivotLine!=i){
      mat[i,] = mat[i,]-(mat[i,pivotColumn]*mat[pivotLine,])
    }
  }
  
  return(mat)
}

print("First simplex resolution")

while(continueCondition(tbl)==0){ #first simplex resolution
  slct <- selection(tbl)
  tbl<-gauss_pivot(tbl,slct[1],slct[2])
  tbl<-renameRows(tbl)
  print(tbl)
}

if(doubleSimplex == 1){#erase artificial variables
  tbl <- cbind(tbl[,1:(ncol(tbl)-1-cptNegBase)],tbl[,ncol(tbl)])
  tbl[nrow(tbl),] <- 0
  nonBase <- nonBaseDetection(tbl)
  nonBase <- append(nonBase,ncol(tbl))
  for(ind in c(1:nbNonBase)){
    posBases <- posBaseDetection(tbl)
    indBase <- which(posBases == ind)
    tbl[nrow(tbl),nonBase] <- tbl[nrow(tbl),nonBase] - C[1,ind] * tbl[indBase[1],nonBase]
  }
  print(tbl)
  
  print("Second simplex : ")
  while(continueCondition(tbl)==0){ #second simplex resolution
    slct <- selection(tbl)
    tbl<-gauss_pivot(tbl,slct[1],slct[2])
    tbl<-renameRows(tbl)
    print(tbl)
  }
}


