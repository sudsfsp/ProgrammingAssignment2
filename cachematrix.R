## Create functions that cache the inverse of a matrix.
## In R, solve () handle only "square" matrix so matrix will 
## be in square matrix format. 

## create argument in makeCacheMatrix function with   
## the help of matrix() format. 

## Function Example

## create matrix when calling makeCacheMatrix function

## a <- makeCacheMatrix (matrix(2,3,2,4),nrow=2,ncol=2)

## cachesolve (a) #cachesolve solves inverse of matrix

##        [,1]  [,2]
## [1,]  2.0   -1
## [2,] -1.5    1

## We verify the answer manually using solve() function
## solve(matrix(c(2,3,2,4),2,2))

## makeCacheMatrix function store list of functions too: 
## set: assign the value of the your new matrix
## get: display the value of the matrix
## set_InvMatrix: set the value of the inverse matrix
## get_Invmatrix: get the value of the inverse matrix

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

##  Calculate the inverse of the special "matrix" created
##  with the abovefunction and should retrieve the
##  inverse from the cache if already available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_InvMatrix()
        if(!is.null(m)) {
                message("getting cache matrix")
                return(m)
        }
        data <- x$get()
        m  <- solve (data)
        x$set_InvMatrix(m)
        m
}
