## This two functions are used to create a special object that stores 
## a matrix, computes its inverse, and cache the inverse.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##set the Matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get the Matrix
  get <- function() x
  ##set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  ##get the inverse of the matrix
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## If the inverse has already been calculated, then the function 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  ## If the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached data")
    ##return the inverse from the cache
    return(i)
  }
  ##else
  ##compute the inverse of the Matrix 'x'
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
