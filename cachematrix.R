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
