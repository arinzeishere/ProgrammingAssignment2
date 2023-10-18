# My version of the codes.
# Here, I create a function for  matrix which can then cache its inverse. This function is makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  #  Here, I initialize the cached inverse to NULL. This cached inverse is denoted by mv
  mv <- NULL
  
  # here, the function sets the matrix and wipes the cached inverse. 
  set <- function(y) {
    x <<- y   
    mv <<- NULL  
  }
  
  # create a fuction, get to get the cureent matrix
  get <- function() x
  
  # create a function here to set the cached inverse. This function is called setInverse
  setInverse <- function(inverse) mv <<- inverse
  
  # create a function to get the cached inverse mv
  getInverse <- function() mv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Create a Function that calculates the inverse of the special "matrix" and  also cache this iverse in the calculations
cacheSolve <- function(x, ...) {
  mv <- x$getInverse()  # this command now retrieve the cached inverse
  
  # create a condition for execution . If there is cached inverse, return the cached inverse and print message denoted in the bracket
  if (!is.null(mv)) {
    message("obtaining cached inverse")
    return(mv)
  }
  
  mat <- x$get()  # this gets the matrix
  mv <- solve(mat, ...)  # Calculates the inverse
  x$setInverse(mv)  # this sets the calculated inverse as cached
  mv  # This returns the inverse
}


# I have added my comments.
