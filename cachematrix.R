## Set the value of the matrix and then return the inverse

## The below sets the values of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        
        ## Assign functions to keywords
        get <- function() x
        setinv <- function(solve) x_inv <<- solve
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The below returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached matrix")
                return(x_inv)
        }
        mat <- x$get()
        x_inv <- solve(mat, ...)
        x$setinv(x_inv)
        x_inv
}

