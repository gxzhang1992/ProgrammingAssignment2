
## This function is used to makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean ##

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the matrix inverse to NULL 
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


## used to get the cache of the matrix
cacheSolve <- function(x, ...) {
  matrixinverse <- x$getmatrixinverse()
  ## if the data exists in cache, it shows "get cache data - unversematrix"
  if(!is.null(matrixinverse)){
    message("get cached data - InverseMatrix")
    return(matrixinverse)
  }
  ## if not, it will calculate and store the data to cache
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setmatrixinverse(matrixinverse)
  matrixinverse
}

