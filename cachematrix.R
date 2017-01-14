
# Below we have two functions: 'makeCacheMatrix' and cacheSolve'.
# The former creates a special matrix object that can cache its inverse
# and the latter calculates the inverse of the said matrix object.



# The function 'makeCacheMatrix' creates a special matrix object
# that can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #Set matrix 'x' to a new matrix 'y' and then set 'inverse' to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #Return matrix 'x'
  get <- function() x
  
  #Set 'inverse' to 'matrixInverse'
  setInverse <- function(matrixInverse) inverse <<- matrixInverse
  
  #Return the inverse of the matrix, 'inverse'
  getInverse <- function() inverse
  
  #Return a vector containing all functions defined above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The function 'cacheSolve' calculates the inverse of the matrix
# object returned by the function 'makeCacheMatrix'.
# If the inverse has already been calculated and there were no changes
# to the matrix, then 'cacheSolve' returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  #Get cached inverse of the matrix 'x', if any
  inverse <- x$getInverse()
  
  #See whether the inverse has already been cached
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #Calculate the inverse of the matrix 'x'
  data <- x$get()
  inverse <- solve(data, ...)
  
  #Save the matrix inverse in the cache
  x$setInverse(inverse)
  
  #Return the inverse of the matrix 'x'
  inverse
}

