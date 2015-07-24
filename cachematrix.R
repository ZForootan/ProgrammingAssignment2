################################################################################
## This set of functions reduces the calculation time of finding the inverse of#
## a matrix by saving the inverse in cache and calling it if needed later.     #
##                                                                             #
## In order to use these sets of functions one has to:                         #
##    1- define the special matrix (MAT) by: MAT<-makeCacheMatrix(x = matrix())#
##                                                                             #
##    2- call the MAT_inv<-cacheSolve(MAT) to calculate the inverse or read it #
##    from cach                                                                #
################################################################################

## This function produces a list of four functions that set and get the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function checks whether the inverse of the called special matrix (made 
## in makeCacheMatrix) is saved in cache or not. If it is the inverse is called 
## from cache, otherwise it uses solve() function to calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}