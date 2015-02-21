## makeCacheMatrix() creates a special matrix which is really a list 
## containing functions to: 
## 	set() - set the value of a matrix, 
## 	get() - get the value of a matrix, 
## 	setinverse() - set (a cached value of) the inverse of a matrix, 
## 	getinverse() - get (a cached value of) the inverse of a matrix.

## cacheSolve()) calculates the inverse of the special matrix created with 
## makeCacheMatrix(), however it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the inverse in the cache.



## makeCacheMatrix() first sets the cache (i) to NULL to initialise. 
##
## It then defines several functions, as follows:
##
## 	set() replaces the matrix (using the global binding of x, as 
## 	makeCacheMatrix() was defined in the global environment) with the 
## 	supplied matrix (y) and resets the cache (the global binding of i, as 
##	makeCacheMatrix() was defined in the global environment) to NULL.
##
## 	get() returns the contents of the matrix (x)
##
## 	setinverse() assigns the passed value to the cache (using the global 
## 	binding of i, as makeCacheMatrix() was defined in the global 
##	environment)
##
## 	getinverse() returns the cache (i)

## Finally, the list assigns names to its members (the defined functions) so 
## that they may be referenced via the $ operator, E.G., x$get

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The cacheSolve function first uses getinverse() to retrieve the
## cache and assign it to i.
## It then checks whether the cache (i) is not NULL, and if so, it returns the
## value of the cache.
## Otherwise, it uses get() to assign the contents of the matrix to data, 
## before using solve() to calculate the inverse of data and assigning to i.
## It then uses the setinverse function to store this calculation in the cache 
## (i)
## Finally, it returns the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
