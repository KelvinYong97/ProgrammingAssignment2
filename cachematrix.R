

# identical to the makeVector function in the example provided
# The first function, makeCacheMatrix creates a special "matrix" that cache's its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inverse) {
        inv <<- inverse
    }
    getinv <- function() {
        inv
    }
    list(set = set,get = get,setinv = setinv,getinv = getinv)
}

# Identical to the cachemean function in the example provided
# The function computes the inverse of the special matrix created.
# If the inverse has been calculated before, it obtain the inverse from the cache and skips to computation
# Else, it will calculated the inverse and place in the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


#testing
testmatrix <- makeCacheMatrix(matrix(c(2,3,2,1,2,1,1,1,2),3,3))
testmatrix$
testmatrix$get()
testmatrix$getinv()
cacheSolve(testmatrix)
cacheSolve(testmatrix)
testmatrix$getinv()
