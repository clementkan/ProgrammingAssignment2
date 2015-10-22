## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## Do note that there are alternatives to matrix inversion. 

## Below are two functions that are used to cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" object that
## can cache its inverse, which is a list containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
        s <- NULL
        
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        
        get <- function(){x}
        
        setinverse <- function(solve){s <<- solve}
        
        getinverse <- function(){s}
        
        list(set = set, 
             get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. However, it first checks to see if the inverse has 
## already been calculated (and the matrix has not changed).If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via the 
## setinverse function.
## Computing the inverse of a square matrix is done using the solve function in R.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached inverse matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}