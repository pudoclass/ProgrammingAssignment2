## Inverse of a matrix is a time consuming function
## We want to store the inverse in memory once and reuse it whenever we need it

## This function takes a matrix as input and returns a list of set and get
## functions that get and set the matrix and its inverse.
## The matrix and its inverse are stored 
## in a variable in the parent frame, using the double assignment operator
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function uses the special matrix object from makeCacheMatrix function
## The input is the list of functions from makeCacheMatrix to set and get 
## the matrix and inverse.  If the inverse is Null, only then in calculates
## inverse.  This saves time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
