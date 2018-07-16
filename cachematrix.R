## These two functions can be used together to set and get a matrix and
## find and return its inverse

## The makeCacheMatrix function sets and gets a matrix
## and sets and gets the inverse of the matrix
makeCacheMatrix <- function(a = matrix()) {
        inver <- NULL
        set <- function(b) {
                a <<- b
                inver <<- NULL
        }
        get <- function() a
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function determines if the inverse of the matrix has been
## computed.  It returns it if it has been computed or will return the value
## in the cache using the setinverse function.

cacheSolve <- function(a, ...) {
        inver <- a$getinverse()
        if(!is.null(inver)) {
                message("Getting cached data.")
                return(inver)
        }
        data <- a$get()
        inver <- solve(data)
        a$setinverse(inver)
        inver
}
## Checking to see if this works, I used the following
## > a = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(a)
## > m$get()
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > cacheSolve(m)
## Received response "Getting cached data."
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## >
