## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# A function is created in which a matrix needs to be fed; this matrix = x 
# this matrix will be associated with a set of functions that will be called by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinv <- function(theinverse) inv <<- theinverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#cacheSolve will serve to retrieve the inverse from the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #either the inverse is already calculated and saved in the object, which then it just needs to be
  #retrieved by using x$getinv(), which brings the value inv, or it needs to be calculated and saved
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting inverse")
    return(inv)
  }
  # when it needs to be calculated, the original matrix that was fed to the makeCacheMatrix object will be 
  # associated with the variable data, then using the solve function the inverse will be calculated and associated 
  # with the variable inv, then it will be saved in the makeCacheMatrix object by using x$setinv(inv)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

myMatrix <- matrix(4:7, nrow = 2, ncol = 2)

MyCacheMatrix <- makeCacheMatrix(myMatrix)

cacheSolve(MyCacheMatrix)
