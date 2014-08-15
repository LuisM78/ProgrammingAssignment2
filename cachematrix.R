## The two functions are used to  pre process the inverse of a
## large matrix, and cache the result of the inversion. This is done
## in order not to have to invert the matrix again.


##
## The first function takes a matrix and creates four functions
## in order to save and extract information about it
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

##
## This second function calculates the inverse of the matrix, assuming there is an inverse.
## If the inversion was already cached, then just returns the inverse value.
## 


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

