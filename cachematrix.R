## In this assignment we have 02 functions that help us to make a matrix, 
## checking its invertibility, and help us to calculate the inverse of the matrix,
## storing it in the cache for next callings, achieving this way the optimization 
## of the computing resources.


## The makeCacheMatrix function has 04 subfunctions that, (1) set a matrix that has
## an inverse -setmatrix- (2) print the invertible matrix -getmatrix- (3) set the 
## inverted matrix called by the cacheSolve function -setinverse- (4) print the 
## inverted matrix -getinverse-
 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setmatrix <- function (y,n){
                if(det(matrix(y,n,n))==0){
                        message("This is not an invertible matrix")
                        x <<- matrix()
                }
                else {
                        x <<- matrix(y,n,n)       
                }
                inv <<- NULL
        }        
        getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse        
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function evaluates if there is an inverted matrix stored in the cache, if it does,
## then returns its value, and if it doesn't, calculates the inverse of the matrix, calls the setinverse
## subfuncion of the makeCacheMatrix function to store the inverted matrix in the object called "inv",
## and finally prints the inverted matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("the inverted matrix is in the cache")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data)
        x$setinverse(inv)
        inv       
}

