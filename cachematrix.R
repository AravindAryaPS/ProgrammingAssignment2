  
#Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly 
#Your assignment is to write a pair of functions that cache the 
#inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <-NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() {x}
  setmean <- function(mean) {inv <<- mean}
  getmean <- function() {inv}
  list( set = set, get = get,setmean = setmean,getmean = getmean)
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.


CacheSolve <- function(x, ...){
  inv <- x$getmean()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setmean(inv)
  inv
}
