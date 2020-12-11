makeCacheMatrix <- function(x = matrix()){
## The following function creates a special "matrix" 
## object that can cache its inverse.
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function()s
  list(set =set , get= get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
