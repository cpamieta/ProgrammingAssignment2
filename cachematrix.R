## Put comments here that give an overall description of what your
## functions do

#The main function of these two functions is to make sure that it is not recalculating a matrix it already did before.
#If it is not a matrix that was not calculated, then it does the inverse and stores it for later use.

## Write a short comment describing this function
#The set function sets the user defined matrix to X
#the get function returns the matrix
#The setinverse and getinverse does a similar task as set and get.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#this function first checked if the matrix was already calculated, it does that by calling the getinverse function.
#If it was not calculated, then this function calculates the inverse of the matrix, then calls the setinverse function to store that value.
cacheSolve <- function(x, ...) {
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
