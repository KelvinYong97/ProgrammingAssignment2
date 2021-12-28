
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
testmatrix$get()
testmatrix$getinv()
cacheSolve(testmatrix)
cacheSolve(testmatrix)
testmatrix$getinv()
