#these pair of functions cache the inverse of a matrix.
#First function creates the matrix which can cache the inverse
#while the second function calculates the inverse matrix.


#creates the cache matrix which has functions to set the value of
#the matrix, get the value of the matrix, set the inverse matrix,
#and get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#calculates the inverse matrix from the "makeCacheMatrix" function.
#function first checks if the inverse has already been calculated.
#If not, it calculates the inverse matrix and sets the inverse
#matrix with setinverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
