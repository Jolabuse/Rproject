tbl = read.delim(file.choose(),header = FALSE,sep=' ') #store in a table the input file
tbl

addBaseCol <- function(tbl,ind){ #insert a base variable for the ind row at the end of A
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1]<-1
  tbl<-cbind(tbl[,1:(ncol(tbl)-2)],bCol,tbl[,(ncol(tbl)-1):ncol(tbl)])
}

addInvBaseCol <- function(tbl,ind){ #invert the values of the ind row and insert a base variable for it at the end of A
  tbl[ind,1:(ncol(tbl)-2)]<- tbl[ind,1:(ncol(tbl)-2)]*-1
  tbl[ind,ncol(tbl)]<- tbl[ind,ncol(tbl)]*-1
  bCol <-matrix(0,nrow(tbl),1)
  bCol[ind,1]<-1
  tbl<-cbind(tbl[,1:(ncol(tbl)-2)],bCol,tbl[,(ncol(tbl)-1):ncol(tbl)])
}


for (ind in c(1:nrow(tbl))) { #create the table
  if(tbl[ind,(ncol(tbl)-1)] == -1){
      tbl <- addBaseCol(tbl,ind)
  }
  if(tbl[ind,(ncol(tbl)-1)] == 1){
  
         tbl <- addInvBaseCol(tbl,ind)
  }
}

tbl<- cbind(tbl[,1:(ncol(tbl)-2)],tbl[,(ncol(tbl))])#erase the inequality column
tbl[nrow(tbl),ncol(tbl)]<-0 #put the 0 for the Z-0
names=c()  #rename the col
for (ind in c(1:(ncol(tbl)-1))) {
  names<-append(names,paste("x",ind,sep=""))
}
names <-append(names,"B")
colnames(tbl)<-names

tbl
