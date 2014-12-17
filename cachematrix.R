makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL

	set <- function(y) {
    	x <<- y
    	m <<- NULL
	}

	get <- function() {
		x
	}

	setinverse <- function(inverse) {		
		m <<- inverse
	}

	getinverse <- function() {
		m
	}

	list(set = set, get = get,
       	 setinverse = setinverse,
       	 getinverse = getinverse) 
}

cacheSolve <- function(y, ...){
	
	x <- y$getinverse()

	if (!is.null(x)){

		message("getting cached data")
		return (x)

	}

	data <- y$get()

	m <- solve(data) %*% data

	y$setinverse(m)

	m

}

# Test
mtx <- matrix(1:4, 2, 2) 
test <- makeCacheMatrix(mtx)
cacheSolve(test)
cacheSolve(test)