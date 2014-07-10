## Caching the inverse of  a matrix

## Creates a Constructor function - to create  a special "matrix" to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

				## Initialize the inverse property
    				inv <- NULL

    				## Set the value of matrix
    				set <- function( matrix ) {
            							m <<- matrix
            							inv <<- NULL
    							  }

    				## Get the value of matrix
     				 get <- function() {
    							## Return the matrix
    							m
    						   }

    				## Set the inverse of the matrix
    				setInverse <- function(inverse) {
    								    inv <<- inverse
    								}

    				## Get the inverse of the matrix
   				 getInverse <- function() {
        						    ## Return the inverse property
        						    inv
    							  }

    				## Return the list of the methods
    					list(set = set, get = get,
         				setInverse = setInverse,
         				getInverse = getInverse)

					  }


## Calculates the inverse of the special "matrix" created with the above function (makeCacheMatrix).  If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        			## Return a matrix that is the inverse of 'x'
				inv.mat <- x$getInverse()
				
				## Return the inverse if its already set
				    if( !is.null(inv.mat) ) {
				            message("Returning Cached Inverse Matrix")
				            return(inv.mat)
				    }
				
				## Get the matrix 
				mat.data <- x$get()
				
				## Calculate the inverse using matrix multiplication
				inv.mat  <- solve(mat.data) %*% mat.data
				
				## Set the inverse 
				x$setInverse(inv.mat)
				
				## Return the matrix inverse
    				inv.mat 
	
				}
