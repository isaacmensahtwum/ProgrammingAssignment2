# This is a function to cache the inverse of a matrix without 
# necessarily going through the whole computation again


# The first function, makeCacheMatrix, is a matrix which has a list 
# containing functions use to set and get the value of the matrix and also
# to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function (y){
    x <<- y
    invers <<- NULL
  }
  get <- function () {x}
  setInverse <- function(inverse) {invers <<- inverse}
  getInverse <- function() {invers}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


# This last function is the cache function which calculates the inverse of 
# the matrix as set by the above function.It checks for the inverse before 
# caculation it. If it has already been calculated then it gives the message, 
# "getting cached data" followed by the inverse of the matrix. Otherwise, it 
# calculates the inverse of the matrix data and gives the value of the matrix 
# in the cache through the setInverse function.

cacheSolve <- function(x, ...) {
    invers <- x$getInverse()
    if(!is.null(invers)){
      message("getting cache data")
      return(invers)
    }
    mat <- x$get()
    invers <- solve(mat, ...)
    x$setInverse(invers)
    invers

}
