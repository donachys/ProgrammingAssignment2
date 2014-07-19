##functions used to cache matrix inverse operations

##usage example for solution:
## source("cachematrix.R")
## mc <- makeCacheMatrix()
## my_m <- matrix(rnorm(16),4,4) #assume square matrix
## md$set(my_m)
## cacheSolve(mc)
## cacheSolve(mc)

## it is assumed that the matrix is square and invertible

## use this to create your 'special matrix'
## set the matrix object before calling cacheSolve on the 
## 'special matrix'

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function(){
		x
	}
	setinv <- function(inv){
		m <<- inv
	}
	getinv <- function(){
		m
	}
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## when your 'special matrix' is created use this to solve the inverse operation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
        	message("getting cached value")
        	return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinv(m)
        m
}
