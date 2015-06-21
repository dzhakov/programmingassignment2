## two functions that can find the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## create the inverse property
  i <- NULL
  
  ## function to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## function the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## function to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## function to get the inverse of the matrix
  getInverse <- function() {
    ## give the inverse property
    i
  }
  
  ## give a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## find the inverse of the special matrix returned by "makeCacheMatrix"
## above.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
