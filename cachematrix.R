makeCacheMatrix <- function(x = matrix()){
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