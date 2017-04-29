## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates 4 other functions that will be used "conditionally"
# these functions will be called by the cacheSolve function depending on wether the matrix 
# changes, and therefore the mean changes

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#now we are testing the functions to see if they do what we expect
# first step is to create a "square" 2x2 matrix
twobytwo <- matrix(c(1,2,3,4),2,2)

#create the functions to create the inverse of the matrix, 
#and cache the value in a function environment
twobytwoX <- makeCacheMatrix(twobytwo)
cacheSolve(twobytwoX) #inverse returned after computation or from cache if matrix has not changed
