## The following functions demonstrates R's lexical scoping properties and the ability to create functions
## that also act as objects to other functions. R's scoping properties give a function access to both the
## objects within the list created by makeCacheMatrix as well as the original object passed through the 
## function. This is handy in this case because it allows us to retrieve cached data rather than run 
## the function again.

## This function takes a solvable square matrix as an object and outputs a list of functions accessible to
## to the cacheSolve function. It allows values to be captured and saved in cache to be accessed later.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function uses makeCacheMatrix as an object. If a value for the inverse of the original object x was
## already computed, this function retrieves that value and returns it. If there is no value, then this 
## function proceeds to calculate the inverse of the original matrix object and returns the value.

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}
