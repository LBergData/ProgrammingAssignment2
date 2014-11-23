## The following two functions are being submitted:
##	makeCacheMatrix
##		This function prepares the environment for the cacheSolve
##		 function to operate. 
##		It holds a matrix vector and sets up additional 
##		sub-functions (methods) that can be called by the separate 
##		function.
##		It's main purpose is to allow for holding a function result
##		 in memory in case it needs to be reused.
##		This allows a calling function to skip re-running an operation 
##		if it's been run before with the same input.
##
##	cacheSolve 
##		This function has as its basic task to invert a given matrix.
##		However, it also makes use of the makeCacheMatrix function in order to avoid re-doing
##		the matrix inversion if it has already been done for the same matrix.


## This function sets up sub-functions (methods) that can be called
## by an external function (see internal comments).
## It also holds a matrix vector that is passed to it for future use.
## Designed to work with cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {


		# reset output vector to NULL every time makeCacheMatrix is called
InvertMatrix <- NULL    

		# create internal function to store variables
set <- function(y) {
   	    x <<- y
       	    InvertMatrix <<- NULL
       		 }

		# create internal function (method) available to external function call 
		# will return value of vector supplied to makeCacheMatrix
get <- function() { x } 

		# create internal function (method) available to external function call 
		# will store the value passed to it into output vector (using "superassignment")
setInvert <- function(Invert)  { InvertMatrix <<- Invert }

		# internal function (method) available to external function call
		# will return the cached value of output vecotr to calling function
getInvert <- function() { InvertMatrix }

		# set up labels for external function to use
    list(get = get, set = set,               
         setInvert = setInvert,  
         getInvert = getInvert)  

}


## Returns the inversion of a square matrix (x).
## Is designed to work with makeCacheMatrix function which
## allows this function to check first if the matrix inversion
## is already available in memory, and, if so, returns the results
## from memory instead of recalculating the inversion

cacheSolve <- function(x, ...) {
        Invert <- x$getInvert()
        if(!is.null(Invert)) {
                message("getting cached data")
                return(Invert)
        }
        data <- x$get()
        Invert <- solve(data, ...)
        x$setInvert(Invert)
        Invert

}
