mytranspose<-function(x){
  if(length(x)==0){return(x)}
  else{
    
    if(is.matrix(x)){
      y<-matrix(NA,nrow=ncol(x), ncol=nrow(x))
      for(i in 1:nrow(x)){
        for(j in 1:ncol(x)){
          y[j,i]<-x[i,j]
        }
      }
      return(y)
    }
    
    else if(is.vector(x)){
      d<-matrix(x)  
      y<-matrix(NA,nrow=ncol(d), ncol=nrow(d))
      for(i in 1:nrow(d)){
        for(j in 1:ncol(d)){
          y[j,i]<-d[i,j]
        }
      }
      class(y)<-'vector'
      return(y)
    }
    
    else if(is.data.frame(x)){
      y<-matrix(NA,nrow=ncol(x), ncol=nrow(x))
      for(i in 1:nrow(x)){
        for(j in 1:ncol(x)){
          y[j,i]<-x[i,j]
          colnames(y)<-row.names(x)
          row.names(y)<-colnames(x)
        }
      }
      class(y)<-'dataframe'
      return(y)
    }
  }

}

