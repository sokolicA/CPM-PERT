Opr<-c(1,2,3,4)
Pred<-list(c(0),c(1),c(0),c(2,3))

matrika.graf<-function(Opr, Pred, Cas){
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
  return(A)
}
