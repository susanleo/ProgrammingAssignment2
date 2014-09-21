## Sets the value of a matrix
## Gets the value of a matrix
## Sets the value of the inverse
## Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()){
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## Returns a matrix that is the inverse of (x)
## First checks the cache for an existing solution

cacheSolve <- function(x, ...){
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting chached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
