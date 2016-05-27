## R Programming, programming assignment 2, lexical scoping
## Cache the inverse of a square matrix, so that when we need it again, 
## it can be looked up in the cache (faster) rather than recomputed. 
## Assume that the matrix supplied is always invertible.

## - - - - - - - - - - 
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## Computing the inverse of a square matrix can be done 
## with the solve function in R.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    ## Introducing: the <<- operator, which can be used to assign a value 
    ## to an object in an environment that is different from the current 
    ## environment 
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse matrix (using SOLVE in place of 
  ## MEAN in example function provided)
  setmatrix <- function(solve) m <<- solve
  
  ## get the value of the inverse matrix 
  getmatrix <- function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## - - - - - - - - - - -
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## message("in the function cachesolve")
  
  m <- x$getmatrix()
  print(m)
  
  ##first checks to see if the inverse matrix has already been calculated and cached.
  if(!is.null(m)) {
    ##If so, it gets the inverse matrix from the cache and skips the computation
    ##message("getting cached inverse matrix")
    
    ## return inverse matrix
    return(m)
  }
  
  ## if not already calculated, it calculates the inverse of the matrix 
  ## and sets the value of the inverse matrix in the cache via the setmatrix function.
  ##message("not cached, calculating")
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  
  ## return inverse matrix
  m
  
}
