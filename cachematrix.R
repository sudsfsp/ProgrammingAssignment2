## Create functions that cache the inverse of a matrix.
##  We can assume the matrix is invertible, and only square
## matrices are invertible. 

## You can assign "square matrix" or "invertible matrix" 
##  whether in matrix () or c () (vector) format. 

## Function Example

## Create matrix in makeCacheMatrix argument :

## source ("cachematrix.R")
## a <- makeCacheMatrix (matrix(2,3,2,4),nrow=2,ncol=2)
# # OR
## a <- makeCacheMatrix (c(2,3,2,4))

## cacheSolve (a) #cacheSolve compute inverse of matrix

##        [,1]  [,2]
## [1,]  2.0   -1
## [2,] -1.5    1

## We verify the above answer manually using solve() function
## solve(matrix(c(2,3,2,4),2,2))

## makeCacheMatrix function store list of functions too: 
## set: assign the value of the new matrix
## get: display the value of the matrix
## set_InvMatrix: set the value of the inverse matrix
## get_Invmatrix: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { 
                ## Calculate length and square root of x for making 
                ## "Square Matrix" if function argument in vector format
                len <- length (x)
                x <- matrix (x, nrow=len/sqrt(len), ncol= len/sqrt(len)) 
        }
        set_InvMatrix <- function(mat) {
                len2 <- length (mat)
                m <<- matrix (mat, nrow=len2/sqrt(len2), ncol= len2/sqrt(len2))
        }
        get_InvMatrix <- function() m
        list (set = set, 
              get = get,
              set_InvMatrix = set_InvMatrix,
              get_InvMatrix =  get_InvMatrix
        )
}

##  Calculate the inverse of the special "matrix" returned
##  by the above function and if inverse matrix already 
##  calculated then cachesolve retrieve from cache.

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
