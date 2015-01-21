## Creates an object-like list to represent an invertible matrix, with functions for setting and 
## returning matrix values and the matrix inverse, which is cached for faster retrieval

## Returns the matrix 'object'
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse to an empty matrix
  mtxInv <<- matrix()
  
  # Create set/get function definitions to store and retrieve the matrix and its inverse:
  set <- function(y){
    x <<- y
    mtxInv <<- matrix()
  }
  get <- function() x 
  setInv <- function(inv) mtxInv <<- inv
  getInv <- function() mtxInv
  
  # Return the matrix 'object' as a list:
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Returns the stored inverse of the matrix if available; if not, calculates the inverse of 
## the matrix, saves it in the 'object', and returns the new value
cacheSolve <- function(x, ...) {
  cachedInv <- x$getInv()
  
  #the inverse is set to an empty 1x1 matrix whenever the matrix is changed; use this
  # to determine if the cached value needs to be updated
  if(nrow(cachedInv)==1 && ncol(cachedInv)==1 && is.na(cachedInv[1,1])) {
    x$setInv(solve(x$get()))
  }
  
  #return the matrix inverse
  x$getInv()
}  
