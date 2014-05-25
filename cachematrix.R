
## The purpose of these functions is to reduce the
## cost of time required to calculate the inverse 
## matrix. This is done by storing the result of 
## the solve function in cache.

## This function caches the calculation of the 
## inverse of the matrix x (parameter) and returns
##  a list with four functions: 
## - set: resets the array and clears (null) the inverse matrix 
## - get: returns the matrix 
## - setmi: resets the inverse matrix 
## - getmi: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## matrix of inverse  
		i <- NULL
		
		## set matrix
		set <- function(y) {
				x <<- y
				i <<- NULL ## clear cache
		}
		
		## get matrix
		get <- function() x
		
		## set matrix of inverse
		setmi <- function(matrix) i <<- matrix
		
		## get matrix of inverse
		getmi <- function() i 
	
		list(set = set, get = get,
             setmi = setmi,
             getmi = getmi)
		
}

## This function checks if the inverse of
## the matrix of the x object  was already 
## cached, if the value of the inverse of 
## the matrix is null then it is calculated 
## and cached. The return of the function 
## is the inverse of the matrix.

cacheSolve <- function(x, ...) {	
		i <- x$getmi()
		if(!is.null(i)) { 
                message("getting cached data")
                return(i)				
		}
		
		matrix <- x$get()
		i <- solve(matrix)
		x$setmi(i)
		
        ## Return a matrix that is the inverse of 'x'
		i
}       
         