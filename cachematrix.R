## The two functions are used to take pre process the inverse of a
## large matrix, and store the result without having to process the inverse
## again

## The first function takes a matrix and creates four functions
## in order to save extract and save information about it
##



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## Gets the values of the matrix
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}

## This function calculates the inverse of the matrix, assuming there is an inverse
## if the value was already stored in the list, just returns the value
## of the matrix that is cached. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting the inverse cached")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  ## function returns the inverse of the matrix
}

