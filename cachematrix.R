#  1. makeCacheMatrix   - Creates a special "matrix" object that can cache its inverse.
#  2. cacheSolve        - Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#                         If the inverse has already been calculated (and the matrix has not changed), 
#                         then the cachesolve should retrieve the inverse from the cache.
#  3. TestIt            - Test it with some special cases.
 makeCacheMatrix <- function(x = matrix()) {
############################################
### - Creates a special "matrix" object that can cache its inverse.
#########################################################################################################
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
############################################
### - Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#   - If the inverse has already been calculated (and the matrix has not changed), 
#         then the cachesolve should retrieve the inverse from the cache.
#   - Returns a  [0] matrix for non-inveratble / non-square etc. matrix
##########################################################################################################

        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data...solved last time")
                return(inv)
        }
###Else...calculate the invese:		
        m<-x$get()
        Dims <- dim(m)
        if( !is.matrix(m) || length(Dims) != 2  ||  Dims[1] != Dims[2]) {
             message ("NOT a square matrix")
			       ret0 <- matrix(0)
			       x$setInverse(ret0)             
             return (ret0)
        } 
		###if (det(m) ==0){
########Use _this_ instead of last line...else you may see an error like "R system is computationally singular"...
########..and yes, rounding to 10 decimal places should be good...right?
        if(round(det(m), digits=10) == 0) {    
             message ("CANNOT be inverted")
             ret0 <- matrix(0)
             x$setInverse(ret0)             
             return (ret0)
		  }		
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

TestIt <- function() {
############################################
### - Simple test function - tries to test it with few simple cases.
#########################################################################################################

		 x=cbind(c(1,2),c(2,3))
		 y <- cbind(c(1,2,3),c(2,3,4),c(3,4,5))
		 z <- cbind(c(1,1,2,1),c(2,2,3,2),c(3,3,4,3),c(5,4,5,4))
		 i=matrix(1)
		 o=matrix(0)
		 
	##	 
		 cx<-makeCacheMatrix(x)
		 cy<-makeCacheMatrix(y)
		 cz<-makeCacheMatrix(z)

		 ci<-makeCacheMatrix(i)
		 co<-makeCacheMatrix(o)	 
	##
	     print("x:"); print(cacheSolve(cx))
		 print("y:"); print(cacheSolve(cy))
		 print("z:"); print(cacheSolve(cz))
		 print("i:"); print(cacheSolve(ci))
		 print("o:"); print(cacheSolve(co))
		 message("--------Everything should be fetched from cache")
	     print("x:"); print(cacheSolve(cx))
		 print("y:"); print(cacheSolve(cy))
		 print("z:"); print(cacheSolve(cz))
		 print("i:"); print(cacheSolve(ci))
		 print("o:"); print(cacheSolve(co))
##
		 message("-------------->Change....... cx, ci and co shoudl be fetched from cache")
		 cy<-makeCacheMatrix(y)
		 cz<-makeCacheMatrix(z)
##
	     print("x:"); print(cacheSolve(cx))
		 print("y:"); print(cacheSolve(cy))
		 print("z:"); print(cacheSolve(cz))
		 print("i:"); print(cacheSolve(ci))
		 print("o:"); print(cacheSolve(co))
		 
		 
}
############################End of program#########################################