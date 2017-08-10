## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function is designed to take a square invertible matrix as an input
#and save this to another environment as well as create a list containing
#functions to retrieve the matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#This function is designed to take in the list of functions produced by 
#makeCacheMatrix and return the inverse of a matrix. It only computes the
#inverse if it has not been computed previously. Otherwise it looks up the
#value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix,...)
	x$setinverse(inverse)
	inverse
}
