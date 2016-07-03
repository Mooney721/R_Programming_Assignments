## The function makeCacheMatrix creates a special matrix, which is a list containing
## a function to do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

## The main focus of makeCacheMatrix is to create a matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## sets the value of the matrix (as NULL)
  set <- function(y) {  
    x <<- y
    m <<- NULL  ## gets the value of the matrix
  }
  get <- function() x
  setinv <- function(solve) m <<- solve  ## sets the value of the inverse of the matrix
  getinv <- function() m  ## gets the value of the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following cacheSolve calculates the matrix inverse of the special matrix 
## created with function makeCacheMatrix. 
## It first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## If not, it calculates the matrix inverse of the data 
## and sets the value of the matrix inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        
    m <- x$getinv() 
    if(!is.null(m)) {  ## checking to see if the matrix inverse has already been calculated
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m ## Returns a matrix that is the inverse of 'x'
  }
