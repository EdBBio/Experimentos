EcoPop<-function(lambda=L,N0=n0,Nt=nt,Nt1=nt1,R=r,T=t,method){
  A<-function(L,n0,t){
    LambdaT<-L^t
    nt<-LambdaT*n0
    return(nt)
  }
  B<-function(nt,nt1){
    LambdaA<-nt1/nt
    return(LambdaA)
  }
  C<-function(nt1,nt,t){
    taxa<-nt1/nt
    Prod<-prod(taxa)
    return(Prod)
  }
  D<-function(n0,r,t){
    R<-r*t
    Exp<-exp(R)
    NT<-n0*Exp
    return(NT)
  }
  if(method=="Nt"){
    return(A)
  } else if(method=="lambda"){
    return(B)
  } else if(method=="T.C.N.C"){
    return(C)
  } else if(method=="C.E.C"){
    return(D)
  }
}

EcoPop(NT1=1,NT=20,T=8,method="T.C.N.C")

A<-function(L,n0,t){
  LambdaT<-L^t
  nt<-LambdaT*n0
  return(nt)
}

A(L=1,n0=20,t=8)
