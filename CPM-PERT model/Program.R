Opr<-c(1,2,3,4)
Pred<-list(c(0),c(1),c(0),c(2,3))
Opr1<-(1:12)
Pred1<-list(c(0),c(0),c(0),c(1,2),c(4),c(3),c(5,6),c(7),c(8),c(7),c(10),c(10))
Adj<-function(Opr, Pred){
  z=length(Opr)+1
  NO<-c(0, Opr,z)
  A<-matrix(nrow=z+1,ncol=z+1)
  O<-Opr
  for (i in Opr){
    p<-Pred[i]
    for (j in p){
     A[j+1,i+1]<-1
     O[!O %in% p]
    }
  }
  for(i in 1:(z+1)){
    for(j in 1:(z+1)){
      if(is.na(A[i,j])){
        A[i,j]<-0
      }
    }
  }
  for(i in 1:z){
    if(sum(A[i,])==0){
      A[i,z+1]<-1
    }
  }
    
  return(A)
}
top.sort<-function(Opr,adj){
  Q<-list()
  i<-1
  st<-c()
  for(v in Opr){
    st[v]<-sum(adj[,v])
  }
  for(v in Opr){
    if (st[v]==0){
      Q[[i]]<-v
      i<-i+1
    }
  }
}








