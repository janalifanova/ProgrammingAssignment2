## makeCacheMatrix: This function creates a list of functions to set/get a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  setmatrix <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function takes a list returned by makeCacheMatrix and uses its functions to 
## check if the inverse matrix was already computed. If not, computes and stores it in the cache.
## Returns the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  if(!is.null(m)){ 
    return(m)
  }
  
  y <- x$getmatrix() 
  m <- solve(y, ...) 
  x$setinverse(m) 
  m 
  
}
