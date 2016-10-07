
makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(solve) matrixinverse <<- solve
  getmatrixinverse <- function() matrixinverse
  list( set = set, get = get,
        setmatrixinverse = setmatrixinverse,
        getmatrixinverse = getmatrixinverse)
}



cacheSolve <- function(x, ...) {
  matrixinverse <- x$getmatrixinverse()
  if(!is.null(matrixinverse)){
    message("get cached data - InverseMatrix")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setmatrixinverse(matrixinverse)
  matrixinverse
}

