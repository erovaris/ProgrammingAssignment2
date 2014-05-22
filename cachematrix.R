##setwd("C:/Users/eduardo/Documents/GitHub/ProgrammingAssignment2")
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		d <- dim(x)
		if (d[1] != d[2]) {				
				stop("The matrix has to be square")
		}

        ## matrix of inverse  
		i <- NULL
		
		## set matrix
		set <- function(y) {
				x <<- y
				i <<- NULL
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

## Write a short comment describing this function

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
         
