## Create a pair of functions that will together cache
## the inverse of an invertible matrix.


### These two functions operate as a pair:

## create a "list" with a function that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
	#1. set the value of a matrix
	m <- NULL

	set <- function(y)
	{
		#assign y in function to x outside of function
		x <<- y
		m <<- NULL
	}

	#2. retrieve the matrix
	get <- function() x 

	#3. set the value of the inverse
	setinverse <- function(inverse) m <<- inverse

	#4. get the value of the inverse
	getinverse <- function() m

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of matrix 'x'
## cache the inverse matrix 

cacheSolve <- function(x, ...) 
{
       
	#see if inverse has been calculated
	inv <- x$getinverse()

	#if the inverse has been calculated previously
	if(!is.null(inv))
	{
		#get the cached inverse 
		message("getting cached data")
		return(inv)
	}

	#gets the matrix
	data <- x$get()

	#calculate the inverse of the matrix
	inv <- solve(data, ...)

	#set the inverse and writes it into x
	x$setinverse(inv)

	#output the inverse 
	inv
}
