## makeCacheMatrix and cacheSolve are a pair of functions that allow a user to take a matrix, store
## the inverse of that matrix, and easily print the cached inverse.

## makeCacheMatrix creates a list of functions to set and retrieve the value of a matrix and the value of 
## it's inverse.

makeCacheMatrix <- function(x = matrix()) {    
        m <- NULL                                               ##defining m within the function
        set <- function(y) {                                    
                x <<- y                                         ##use of <<- leaves changes in global envir
                m <<- NULL                                      ##whenever set is run, m reset to NULL
        }                
        get <- function() x                                     
        setinverse <- function(inverse) m <<- inverse           ##whenever setinverse is run, m reassigned
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the matrix from before and checks if the inverse has previously been calculated, and 
## if so it will print the cached inverse. If not, the inverse is calculated and cached.

cacheSolve <- function(x,...) {
        m <- x$getinverse()                                     ##pulls global envir m value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)                                       ##print if inverse already cached
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)                                         ##uses fcn m to reassign global m
        m
}