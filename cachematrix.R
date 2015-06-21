## makeCacheMatrix takes the  matrix argument makeCacheMatrix(data, nrow,ncol)
## and find the inverse of the matrix when called by cacheSolve function 
## the input should be a non singulat square matrix to find the inverse

## getInverse will hold the inverse once cacheSolve function is executed, 
## otherwise hold NULL value or the value from the cache(previous value).


makeCacheMatrix <- function(x = matrix())
	{
		m <- NULL
		set <- function(y) 
			{
				x <<- y
				m <<- NULL

			}
		
		## Assign the makeCacheMatrix function to get variable
		## get hold the matrix defined in makeCacheMatrix
		get <- function() x

		## set Inverse can reasign another matrix to the function 
		setInverse <- function(FindInverse) m <<- FindInverse

		## 1.getInverse will hold  NULL before setInverse and cacheSolve are called
		## 2.getInverse will hold inverse once cacheSolve function is called
		## 3.getInverse will hold the new matrix if setInverse is set with another matrix
		getInverse <- function() m

		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}
 


## cacheSolve function call the getInverse function of makeCacheMatrix
## and find the inverse using solve() function and return the matrix inverse.

cacheSolve <- function(x, ...) 

	{
       	## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
		if(!is.null(m))
		{
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		## Inverse is calculated by calling solve(data)
		m <- solve(data)
		x$setInverse(m)
		m
	}
