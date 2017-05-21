## Put comments here that give an overall description of what your
## functions do
## This file has 2 functions 
## makeCacheMatrix - will create a list to set/get/setInverse/GetInverse matric
## cacheSolve - will get Inverse Matrix if exists otherwise calculates Inverser

## function makeCacheMatrix will return a list with functions 
##	1) set matrix
##	2) get matrix
##	3) setInverse matrix
##	4) getInverse matrix

makeCacheMatrix <- function(x = matrix()) {
  	inv <- NULL
  	set <- function(y) {
    		x <<- y
    		inv <<- NULL
  		}
  	get <- function() x
# Set Inverse 
  	setInverse <- function(inverse) inv <<- solve(x) #calculate the inverse
  	getInverse <- function() inv
  	list(set = set,
       	get = get,
       	setInverse = setInverse,
       	getInverse = getInverse)

}


## cacheSolve function will calculate matrix inverse by using Solve.
## if matrix Iinverse already exists gets it from cache if not then calculates

cacheSolve <- function(x, ...) {
  	inv<-x$getInverse()
  	if (!is.null(inv)) {
     		message("getting cached data")
     		return(inv)
		}
   	d<-x$get()
   	inv<-solve(d, ...)
   	x$setInverse(inv)
	inv
}
    
       

