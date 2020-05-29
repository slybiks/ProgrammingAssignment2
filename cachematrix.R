## A set of function that stores a numeric matrix and caches its mean

## This function creates a matrix object that can store the cached matrix and defines all the necessary interfaces required to get and set the original matrix as well get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invertedmatrix) inverse <<- invertedmatrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the cached matrix if the inverse has already been computed, otherwise, it computes the inverse and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedmatrix <- x$getinverse()
  if(!is.null(invertedmatrix)){
      message("Retrieving cached data")
      invertedmatrix
  }
  data <- x$get()
  invertedmatrix <- solve(data)
  x$setinverse(invertedmatrix)
  invertedmatrix
}
