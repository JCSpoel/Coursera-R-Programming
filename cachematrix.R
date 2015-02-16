## Thank you for reviewing and feedbacking my assignment, and goodluck on the rest of the course!
## I have built my code based on the example of caching the mean of a vector as given in the assignment, 
## and transformed it to a matrix-based caching method.
## The results are succesfully checked using the 'test code' as posted by Jules Stuifbergen in the Coursera Forum, and came out as correct.

## In accordance with the 'makeVector' example in the assignment, I have also built a function that 'sets' and 'gets' the value of a matrix 
## ("set" and "get" respectively), and than 'sets' and 'gets' the inverse of the matrix ("setinverse"and "getinverse", respectively)


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## The following function checks whether an inverse has already been calculated. If this is the case, the message
## shows and the inverse is returned. If this is not the case, the inverse is calculated, and the value is cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## The results are checked using the testcode, and appear to be correct. If you have comments or tips, please tell me.
## Thanks again for evaluating my output and helping me out. Goodluck in the rest of the course, and enjoy R statistics!
