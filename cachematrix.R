## The functions in this script create a special object that can store a 
## square matrix, compute and then cache its inverse. The functions can
## check if an inverse has been calculated already and return that inverse or
## calculate an inverse if no inverse has been calculated or if the matrix 
## provided is different from the matrix whose inverse is cached

## The makecachematrix sets and gets the matrix as well as sets and gets the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setcache <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setcache=setcache, getinverse=getinverse)
}

## This function checks if the inverse for the specific matrix has been 
## calculated and returns it; if the inverse has not been calculated, it
## calculates the inverse and sets the value of the cache  via the setcache 
## function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message ("Getting cached data...")
    return(inv)
  }
  tobesolved <- x$getmatrix()
  inv <- solve(tobesolved, ...)
  x$setcache(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
