makeCachematrix<- function(x=matrix()){   #cachemmatrix stores the inverse of passed matrix
  inv<- NULL
  set <-function(y){
    x<<-y
    inv<<- NULL
  }
  get<- function(){x}
  setInverse<-function(inverse){inv<<- inverse}
  getInverse<- function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)  
  
}

cachesolve<- function(x, ...){  #checks whether the matrix has already been passed and it's inverse is stored in cache
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv<-solve(mat,...) #solve(a,b), solves the equation a%*%x=b i.e gives theinverse 
  x$setInverse(inv)
  inv
}