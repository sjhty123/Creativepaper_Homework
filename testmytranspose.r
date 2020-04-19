source('mytranspose.r')


assertEqual<-function(enter,expect){
  if(all.equal(enter,expect)==TRUE){TRUE}
  else{FALSE}
}

###1
myvar1<-matrix(1:10, nrow=5,ncol=2)

expect<-t(myvar1)
class(expect)<-'matrix'
print(expect)
print(mytranspose(myvar1))

assertEqual(mytranspose(myvar1),expect)

###2
myvar1<-matrix(NA, nrow=0,ncol=0)

expect<-t(myvar1)
class(expect)<-'matrix'
print(expect)
print(mytranspose(myvar1))

assertEqual(mytranspose(myvar1),expect)


###3
myvar1<-matrix(c(1,2), nrow=1,ncol=2)

expect<-t(myvar1)
class(expect)<-'matrix'
print(expect)
print(mytranspose(myvar1))

assertEqual(mytranspose(myvar1),expect)

###4
myvar1<-matrix(c(1,2),nrow=2,ncol=1)

expect<-t(myvar1)
class(expect)<-'matrix'
print(expect)
print(mytranspose(myvar1))

assertEqual(mytranspose(myvar1),expect)


###5
myvar2<-c(1,2,NA,3)

expect<-t(myvar2)
class(expect)<-'vector'
print(expect)
print(mytranspose(myvar2))

assertEqual(mytranspose(myvar2),expect)

###6
myvar2<-c(NA)

expect<-t(myvar2)
class(expect)<-'vector'
print(expect)
print(mytranspose(myvar2))

assertEqual(mytranspose(myvar2),expect)

###7
myvar2<-c()

expect<-NULL
print(expect)
print(mytranspose(myvar2))

assertEqual(mytranspose(myvar2),expect)


###8
d<-c(1,2,3,4)
e<-c('red','white','red',NA)
f<-c(TRUE,TRUE,TRUE,TRUE)
mydata3<-data.frame(d,e,f)

expect<-t(mydata3)
colnames(expect)<-row.names(mydata3)
class(expect)<-'dataframe'
print(expect)
print(mytranspose(mydata3))

assertEqual(mytranspose(mydata3),expect)
all.equal(mytranspose(mydata3),expect)

