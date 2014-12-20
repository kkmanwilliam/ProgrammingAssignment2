# In this function, there're 2 variable. 
# X is the variable we're handling right now, and m is for output storage.
makeCacheMatrix <- function(x = matrix()) {	
	
	m <- NULL

	# " <<- "works on the global environment instead of the local ones 
	set <- function(y) {
    	x <<- y 
    	m <<- NULL
	}
	get <- function() {
		x
	}
	# Output storage
	setinverse <- function(inverse) {		
		m <<- inverse
	}
	# Call for storaged output
	getinverse <- function() {
		m
	}
	# the function will return a list, in which contains 4 small function we defined earlier
	list(set = set, get = get,
       	 setinverse = setinverse,
       	 getinverse = getinverse) 
}

# the function's where the calculation happend!
# the input should be a "makeCacheMatrix" 
cacheSolve <- function(y, ...){

	# Call for storaged output
	x <- y$getinverse()

	# if there's data storaged, return the data
	if (!is.null(x)){
		message("getting cached data")
		return (x)
	}

	# calling the matrix we're about to handle, and start calculation
	data <- y$get()
	m <- solve(data) %*% data

	# Output storage
	y$setinverse(m)

	# Return the answer
	m

}

##################
#    Test Code   #
##################
mtx <- matrix(1:4, 2, 2) 
test <- makeCacheMatrix(mtx)
cacheSolve(test)
cacheSolve(test)