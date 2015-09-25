## These two functions work together to cache the result of a call to solve()


## holds a cached reference to an inverse of a matrix
makeCacheMatrix <- function(myMatrix = matrix()) {
     inverseMatrix <- NULL
     
     set <- function(y) {
          myMatrix <<- y
          inverseMatrix <<- NULL
     }
     get <- function() myMatrix
     setinverse <- function(inverse) inverseMatrix <<- inverse
     getinverse <- function() inverseMatrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## input x is an object created using makeCacheMatrix
## this function will return the cached value of the input object, if available.
## If the cached value is not available, it will calculate it and set it on the object
## so it will be available next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverseMatrix <- x$getinverse()
     if(!is.null(inverseMatrix)) {
          message("getting cached data")
          return(inverseMatrix)
     }
     inverseMatrix <- solve(x$get(), ...)
     x$setinverse(inverseMatrix)
     inverseMatrix
}
