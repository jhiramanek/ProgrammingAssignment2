## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
        Set <- function(aMatrix) {
                x <<- aMatrix
                matrixInverse <<- NULL
        }
        Get <- function() x
        SetInverse <- function(inverse) matrixInverse <<- inverse
        GetInverse <- function() matrixInverse
        list(set = Set, get = Get,
               setinverse = SetInverse,
               getinverse = GetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
		 matrixInverse <- x$getinverse()
            if(!is.null(matrixInverse)) {
                message("getting cached inverse")
                return(matrixInverse)
	     }
		
		 data <- x$get()
		 matrixInverse <- solve(data, ...) %*% data
		 x$setinverse(matrixInverse)
		
		 ## Return a matrix that is the inverse of 'x'
		 matrixInverse
    }
