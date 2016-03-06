## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions that cache the inverse of a matrix.


## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	set <- function(y) {
	                x <<- y
	                invMatrix <<- NULL
  			   }

	get <- function() x
    	setinverse <- function(inverse) invMatrix <<- inverse
        getinverse <- function() invMatrix
	    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function


## The following function calculates the inverese of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invMatrix <- x$getinverse()
    	if(!is.null(invMatrix)) {
            			message("getting cached data.")
	    			return(invMatrix)
		       		}
	data <- x$get()
	invMatrix <- solve(data)
	x$setinverse(invMatrix)
	invMatrix


}
