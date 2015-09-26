## Caching the inverse of a matrix
## We want to calculate the inverse of a matrix
## Instead of calculating repeatedly the program will keep a cache and check it
## The first function will create a object storing the matrix and its inverse
## The second one will solve the inverse of the matrix

## This function will create a special matrix
## The matrix contains the value of the matrix and its inverse

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


## This function checks if the inverse was already calculated and if not, it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
