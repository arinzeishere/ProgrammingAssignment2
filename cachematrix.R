#my comments
# create a function makeCacheMatrix, this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function() {
  # here I initialize matrix and inverse cache
  mat <- NULL
  inverse <- NULL
  
  # set function for the matrix
  set <- function(matrix) {
    mat <<- matrix  
    inverse <<- NULL  
  }
  
  # get function for the matrix
  get <- function() mat
  
  # a function to get the inverse
  getInverse <- function() inverse
  
  # a function to calculate and cache the inverse
  cacheSolve <- function(solve = TRUE) {
    if (!is.null(inverse) && !solve) {
      message("Retrieving cached inverse")
      return(inverse)  # Return the cached inverse 
    }
    
    if (is.null(mat)) {
      stop("Matrix is not set.")
    }
    
    message("Calculating inverse...")
    inverse <- solve(mat)
    
    if (solve) {
      this$setInverse(inverse)
    }
    
    return(inverse)  # Return the inverse
  }
  
  
  return(list(set = set, get = get, getInverse = getInverse, cacheSolve = cacheSolve))
}


cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()  # Confirm  if the inverse has been cached
  if (!is.null(inverse)) {
    message("Retrieving cached inverse")
    return(inverse)  # Return cached inverse 
  }
  
  message("Computing inverse...")
  inverse <- x$cacheSolve()  # Calculate and cache the inverse
  return(inverse)  # produce the inverse
}

#I have added my comments and now commiting changes

