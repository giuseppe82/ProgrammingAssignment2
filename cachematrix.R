## Programming Assignment 2 - R Programming 
## Data Science Specialization Track 
## Assignment: Caching the Inverse of a Matrix

# The first function, makeCacheMatrix, creates a special "matrix", 
# which is really a list containing the functions to:
# a. set the value of the Matrix
# b. get the value of the Matrix
# c. set the value of the Inverse Matrix
# d. get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
		INV <- NULL  # initially set to NULL 
		set <- function(y){
			x <<- y
			INV <<- NULL
		}
		get <- function() x   # get the matrix itself 
		setinverse <- function(solve) INV <<- solve
		getinverse <- function() INV  # get the inverse matrix
		list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
}

# The following function, cacheSolve, calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse 
# of the data and sets the value of the inverse in the cache via the 
# setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	INV <- x$getinverse()
	if(!is.null(INV)) {
		message("getting cached matrix")
        return(INV)
    }
    data <- x$get()
    INV  <- solve(data, ...)
    x$setinverse(INV)
    INV
}

# Usage example:  
# > x <- matrix(rnorm(9), norw = 3, ncol = 3)
# > Cx <- makeCacheMatrix(x)                  // create our special matrix Cx
# > Cx$get()                                  // return the matrix Cx
# > m  <- cacheSolve(Cx)                      // Return the inverse of Cx
# > m2 <- cacheSolve(Cx)                      // Call the 2nd time, so return
#                                             // the cached inverse
