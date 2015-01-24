## We want to cache the inverse of a given matrix in order
## to save time in the computation

## This first fuction is a list of four functions: one to set
## the matrix we want to invert, one to get that matrix, one to set
## the inverse matrix, one to get that inverse matrix

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(matrix) inv<<-matrix
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}



## This function calculates the inverse (by means of the R 
## function solve) of the matrix. It first checks if the inverse 
## has already been calculated and stored. In that case, the 
## calculation is skipped and the cached inverse matrix is returned.
## Otherwise, the inverse matrix is evauated and cached.

cacheSolve<-function(x=matrix(),...){
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting chache data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix,...)
  x$setinv(inv)
  inv
}
