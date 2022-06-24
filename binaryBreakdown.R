library(dplyr)


opposeLine <- function(tbl){#if you have a greater than you take the opposite of the line
  for(ind in c(1:nrow(tbl))){
    if(tbl[ind,(ncol(tbl)-1)] == 1){
      tbl[ind,] <- -1 * tbl[ind,]
    }
  }
  return(tbl)
}

binaryEquivalent <- function(x){   #find the exposant of two that expresses the int given in input 
  binaryExposant<-0
  binaryMax <- 1
  while(binaryMax<x){
    binaryExposant<-binaryExposant+1
    binaryMax <- binaryMax + 2^binaryExposant
  }
  return(binaryExposant)
}



upperBandBinaryEquivalent <- function(upperBand){    #for each variable constraint we create a table for each 2^ 
  upperBandMat <- data.frame()                       #from 0 to the one found in binaryEquivalent
  for(ind in c(1:length(upperBand))){
    binaryExposant <- binaryEquivalent(upperBand[ind])
    upperBandCol <- c()
    for(ind2 in c(0:binaryExposant)){
      upperBandCol <- append(upperBandCol,(2^ind2))
    }
    upperBandCol <- data.frame(t(upperBandCol))
    upperBandMat<- bind_rows(upperBandMat,upperBandCol)  #we use bind_rows to adapt the ncol of the database 
    print(upperBandMat)                                  #and fill it with NA
    print("--------------------------------")
  }
  return(upperBandMat)
}


binaryBreakdown <- function(tbl,upBandBinEq,nbVar){  #we do the binary transformation of each x 
  newTbl <- data.frame()                             #in function of its constraint
  for(ind in c(1:(nrow(tbl)))){
    newcol <- c()
    for(ind1 in c(1:nbVar)){
      for(ind2 in c(1:ncol(upBandBinEq))){
        if(!is.na(upBandBinEq[ind1,ind2])){
          newcol<-append(newcol,(tbl[ind,ind1]*upBandBinEq[ind1,ind2]))
        }
      }
    }
    newcol<-data.frame(t(newcol))
    newTbl <- bind_rows(newTbl,newcol)
  }
  
  newTbl<-bind_cols(newTbl,tbl[,(nbVar+1):ncol(tbl)])
  return(newTbl)
}






tbl <- read.delim(file.choose(), header = FALSE, sep = " ") # store in a table the input file
nbVar <- ncol(tbl)-2
upperBand <- tbl[(nrow(tbl)-nbVar+1):nrow(tbl),1]   #we save the constraints for the variables in a table
tbl <- tbl[1:(nrow(tbl)-nbVar),]                    #so we erase them from the main table
tbl <- opposeLine(tbl)                              #we finalise the set up of the table
print(tbl)
print(upperBand)
upBandBinEq<-upperBandBinaryEquivalent(upperBand)   #we find the binary equivalent of the constraints for the variables
tbl<-binaryBreakdown(tbl,upBandBinEq,nbVar)         #we do the binary breakdown
print("End of Binary Breakdown")
binaryTree(tbl)#don't forget to first run the binaryTree.r file

