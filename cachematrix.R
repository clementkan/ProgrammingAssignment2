## The following two functions are used to create a special object that stores a matrix 
## and cache's its inverse (inverse of the matrix).

## The first function, makeCacheMatrix creates a special "matrix", 
## which is just a list containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() {x}
        
        setinverse <- function(inverse) {s <<- inverse}
        
        getinverse <- function() {s}
        
        list(set = set, 
             get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## The second function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## However, it first checks to see if the inverse matrix has already been computed. If so, it gets the 
## inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix 
## and sets the value of the inverse matrix in the cache via the setinverse function.
## Computing the inverse of a square matrix is done using the solve function in R.

cacheSolve <- function(x, ...){
        
        ## checks to see if the inverse matrix has already been computed. If yes, it gets the 
        ## inverse matrix from the cache and skips the computation.
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached of inverse matrix")
                return(s)
        }
        
        ## Inverse matrix not available, hence it calculates the inverse matrix and sets the 
        ## value of the inverse matrix in the cache via the setinverse function.
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        
        s
}
