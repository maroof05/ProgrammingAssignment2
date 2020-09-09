#Goal:To create two functions i.e, makeCacheMatrix and cacheSolve 
#to find Inverse of a matrix.

#makeCacheMatrix: This function allow us to create a matrix object that can cache its Inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m 
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}

#cacheSolve This function computes the Inverse of the matrix returned by makeCacheMatrix function.
#If the Inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#Inverse from the cache



cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)){
    message("cached matrix")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInv(m)
  m      ## Return a matrix that is the Inverse of 'x'
}
