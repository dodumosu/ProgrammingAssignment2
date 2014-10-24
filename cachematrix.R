## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a 'matrix' that caches its inverse
# accessor methods are added to set/get the actual
# underlying data and inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, getinv=getinv, setinv=setinv)

}


## Write a short comment describing this function
# retrieves the inverse of a 'matrix' as returned
# from makeCacheMatrix() above. if the inverse is
# already cached, return the cached inverse,
# otherwise, solve for the inverse, cache the
# inverse, and return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if (!is.null(inverse)) {
    	message('getting cached data')
    	return(inverse)
    }

    realmat <- x$get()
    solved_inv <- solve(realmat)
    x$setinv(solved_inv)
    solved_inv
}
