## The following two functions combined compute the inverse of a matrix while
##returning the inverse from the cache if it has been calculated previously.

## The following function calculates de inverse of a matrix and caches the result
##The function makeCacheMatrix does the following:
##              1.  set the value of the matrix
##              2.  get the value of the matrix
##              3.  set the value of the inverse
##              4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes de inverse f a matrix returned by the makeCacheMatrix function
## If the inverse has already been calculated it returns it from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'



## Test results


testMatrix <- matrix(c(0, 1, 0, 1, 0, 0, 1, 0, 1),3,3)

testInverse <- makeCacheMatrix(testMatrix)
cacheSolve(testInverse)

## When ran it gives the correct inverse of the matrix (c(0, 1, 0, 1, 0, 0, 1, 0, 1),3,3):

##       [,1] [,2] [,3]
##[1,]    0    1    0
##[2,]    1    0   -1
##[3,]    0    0    1

