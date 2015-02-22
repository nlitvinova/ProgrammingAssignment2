## The functions allow to calculate inverse matrix, store it
## and then retreive if user tries to calculate inverse for
## the same matrix

## makeCacheMatrix is a 'backend' functon
## which gets user provided matrix 
## and prepares a list with the elements pointing to
## matrix processing functions: set, get, calculate
## inverse and get this inverse

makeCacheMatrix <- function(x = matrix()) {
	matr <- NULL
	set <- function(y = matrix()) {
		x <<- y
		matr <<- NULL
	}
      get <- function() x
      calcInverse <- function(solve) matr <<- solve
      getInverse <- function() matr
      list(set = set, get = get, calcInverse = calcInverse, getInverse = getInverse)

}


## cacheSolve checks if ther is a cached matrix
## and retreives a cached one if it exists
## otherwise it calls calcInverse to calculate it

cacheSolve <- function(x, ...) {
# check if we have a cached inverse matrix
	matr <- x$getInverse()
	if(!is.null(matr)) {
# if we have inverse one, print message and return stored matr
		message("getting cached inverse matrix")
		return(matr)
	}
# if there is no inverse in matr variable then calculate inverse
	else {
        data <- x$get()
        matr <- solve(data, ...)
        x$calcInverse(matr)
        matr
	}

}
