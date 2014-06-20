## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the cached invesre of the matrix
  
  inv <- NULL
  
  # Setter for the matrix
  
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  # Getter for the inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  # if the invcesre of the matrix is already calculated return it
  
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
  }
  
  #solve the inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  # Return the inverse
  inv
}
