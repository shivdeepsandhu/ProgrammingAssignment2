## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function calculates inverse of a given matrix
## and stores it in the cache to avoid multiple recalculayions

## Write a short comment describing this function
## This function  sets the value of inverse Matrix(inv_mat) to NULL everytime a new 
## value is passed to the function. It defines 4 functions: 
## get = get the value of a matrix
## set = assign or initilaize new values to the matrix
## getinv = to retreive the cached value of the inverse of a matrix
## setinv = assign or store the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      inv_mat<- NULL
      set <- function(y) {
            x <<- y
            inv_mat <<- NULL
      }
      get <- function() x
      setinv <- function(inv) {
            inv_mat <<- inv
      } 
      getinv <- function(){
            inv_mat
      } 
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}

## Write a short comment describing this funtion
## This function gets the cached inverse value(inv_mat) from the parent env.
## In case the value received is NULL, inverse of the supplied matrix is computed
## and set using the serinv() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv_mat <- x$getinv()
      if(!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
      }
      data <- x$get()
      inv_mat <- solve(data, ...)
      x$setinv(inv_mat)
      inv_mat
}
