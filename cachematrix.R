## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	getinverse <- function() inv
	setinverse <- function(inverse) inv <<- inverse
	list(set = set, get = get,
	 setinverse = setinverse,
	 getinverse = getinverse)
}


## uses makeCacheMatrix to get the inverse from the cache if it has already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("hitting cache")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
