#Opr<-c(1,2,3,4)
#stPred<-list(c(0),c(1),c(0),c(2,3))
Opr<-(1:12)
Pred<-list(c(0),c(0),c(1,2),c(2),c(3),c(0),c(5,6),c(7),c(8),c(7),c(10),c(10))
#Funkcija Adj sestavi matriko sosednosti, če imamo podan vektor opravil in seznam sosedov
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
#Topološko urejanje
top.sort<-function(Opr,Pred){
  Q<-c()
  st<-c()
  top.ured<-c()
  adj<-Adj(Opr,Pred)
  z=length(Opr)+1
  NO<-c(1, Opr+1,z+1)
  for(v in NO){
    st[v]<-sum(adj[,v])
  }
  for(v in NO){
    if (st[v]==0){
      Q<-c(Q,v)
    }
  }
  j<-1
  top.ured<-c()
  while(j<(length(Pred)+2)){
    for(u in Q){
      remove<-c(u)
      Q<-Q[!Q %in% remove]
      top.ured[u]<-j
      j<-j+1
      s<-adj[u,]
      for(v in 1:length(s)){
        if(s[v]==1){
          st[v]<-st[v]-1
          if(st[v]==0){
            Q<-c(Q,v)
          }
        }
      }
    }
  }
  
  return(top.ured)
}








