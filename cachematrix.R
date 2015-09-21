## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        set_InvMatrix <- function(mat) m <<- mat
        
        get_InvMatrix <- function() m
        
        list(set = set, get = get,
             set_InvMatrix = set_InvMatrix,
             get_InvMatrix =  get_InvMatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_InvMatrix()
        if(!is.null(m)) {
                message("getting matrix from cache")
                return(m)
        }
        data <- x$get()
        m  <- solve (data)
        x$set_InvMatrix(m)
        m
}
