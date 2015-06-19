## The following functions will inverse a matrix and store it in the cache for multiple uses

## The makeCacheMatrix function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function (y) { ## sets the value of the matrix, if different from the original value
	x <<- y
	I <<- NULL
}

	get <- function()x ## retrieves the value of the original matrix and displays it
	setmatrix <- function(inverse) I <<- inverse ## sets the value of the inverse matrix
	getmatrix <- function() I ## gets the value of the inverse matrix
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The function cacheSolve will calculate the inverse of a "matrix" object returned by makeCacheMatrix above. If the inverse has already been calcuated, the inverse is returned without recalculation. 
## If the inverse has not been calculated already, the function calculates the inverse, stores it and returns it.

cacheSolve <- function(x, ...) {
        
	I <- x$getmatrix()
	if(!is.null(I)) {
	message("getting cached data")
	return(I)
}
	matrix <- x$get()
	I <- solve(matrix, ...)
	x$setmatrix(I)
	I
}
