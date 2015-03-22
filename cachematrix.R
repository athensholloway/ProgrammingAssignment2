## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a user-defined R function that returns a list 
## of functions that perform the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the matrix
## Parameters:
## x is an invertable matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve calculates the inverse of a matrix given a makeCacheMatrix 
## result. cacheSolve first checks to see if a cached inverse calculation
## has been computed. If so, the cached inverse value is used. Otherwise, 
## a new value is computed and cached.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
