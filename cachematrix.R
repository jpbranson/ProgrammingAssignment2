## These functions create a list cache of a matrix and its inverse, as well as checking that cache when prompted 
## to solve further matrices. 

## The first function, makeCacheMatrix creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }



## The following function calculates the inverse of the matrix from the previous function.
## However, it first checks if the inverse has already been calculated. If it has, it uses the get() function
## to bypass computation. Otherwise, it will solve the matrix and store the inverse using setinv().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }

test_matrix <- makeCacheMatrix(test)

cacheSolve(test_matrix)