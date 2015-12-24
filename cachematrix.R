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
    ##sets the matrix entered by user as the matrix to be inverted
    inv <<- NULL
    ## initializes inverse matrix for matrix y as null 
  }
  getmatrix <- function() x
  ## gets the matrix entered 
  setcache <- function(solve) inv <<- solve
  ## set the inverse as the cached value
  getinverse <- function() inv
  ## gets the cached value
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setcache=setcache, getinverse=getinverse)
  ## creates a list of all functions returned by makecachematrix
}

## This function checks if the inverse for the specific matrix has been 
## calculated and returns it; if the inverse has not been calculated, it
## calculates the inverse and sets the value of the cache  via the setcache 
## function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## sets inverse as last cached value
  if (!is.null(inv)){
    message ("Getting cached data...")
    return(inv)
    ## checks and returns inverse only if it is not a null value
  }
  tobesolved <- x$getmatrix()
  ## gets matrix to be inverted from makecachematrix list
  inv <- solve(tobesolved, ...)
  ## calculates the inverse of the matrix using the solve function
  
  x$setcache(inv) ## sets new inverse as the cached inverse
  inv        ## returns a matrix that is the inverse of 'x'
}
