##These functions cache the inverse of a matrix so that the value
##of that inverse can be accessed rather than re-computing the value
##of the matrix every single time you want to use it.


## makeCacheMatrix creates a matrix object that can cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <- NULL
  }
  get <- function()x
  setinverse <- function(solve) m<<- solve
  getinverse <- function()m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix. If it has already been
##computed, this function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
