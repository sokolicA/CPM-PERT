#Opr<-c(1,2,3,4)
#stPred<-list(c(0),c(1),c(0),c(2,3))
Opr<-(1:12)
Pred<-list(c(0),c(1),c(2,7),c(0),c(0),c(4,5),c(6),c(3),c(3),c(8),c(9),c(9))
#Funkcija Adj sestavi matriko sosednosti, če imamo podan vektor opravil in seznam sosedov
Adj<-function(Opr, Pred){
  z=length(Opr)+1
  NO<-c(0, Opr,z) #dodamo opravili "začetek" in "konec"
  A<-matrix(nrow=z+1,ncol=z+1)
  O<-Opr
  #za vse sosede vozlišča na ustrezna mesta v matriki napišemo enice:
  for (i in Opr){
    p<-Pred[i]
    for (j in p){
     A[j+1,i+1]<-1
     O[!O %in% p]
    }
  } 
  #na mesta kjer ni enice napišemo 0:
  for(i in 1:(z+1)){
    for(j in 1:(z+1)){
      if(is.na(A[i,j])){
        A[i,j]<-0
      }
    }
  }
 #opravila, ki niso pogoj za nobeno drugo opravilo "povežemo" z opravilom "konec" 
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
  #zaradi lažjega programiranja, ima vozlišče "začetek" od sedaj dalje številko 1, podana opravila 
  #imajo številke 2 do n+1, vozlišče "konec" pa ima številko n+2 (n je število opravil)
  NO<-c(1, Opr+1,z+1)
  #določimo stopnjo vozlišč
  for(v in NO){
    st[v]<-sum(adj[,v])
  }
  #vozlišča s stopnjo 0 dodamo v vektor Q
  for(v in NO){
    if (st[v]==0){
      Q<-c(Q,v)
    }
  }
  j<-1
  top.ured<-c()
  while(j<(length(Pred)+2)){ 
    for(u in Q){
      remove<-c(u) #
      Q<-Q[!Q %in% remove] #iz vektorja Q izbrišemo obravnavamo vozlišče
      top.ured[u]<-j # obravnavanemu opravilu dodamo zaporedno številko opravljanja
      j<-j+1
      s<-adj[u,] #vektor, ki nam pove za katera opravila je pogoj obravnavano opravilo u
      #znižamo stopnjo vseh vozlišč, katerih pogoj je u, opravila katerih stopnja je 0 dodamo v Q
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
  #dobimo vektor zaporednih številk opravljanja opravil
  return(top.ured)
}








