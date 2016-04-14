## This Script contains two functions that cache the inverse of a matrix. 
## The idea is that once the inverse is calculated its stored into the cache,
## so there is no need to calculate it again

## This function creates a special "matrix" object that can cache its inverse. 
## The function's output is a list of functions that can: 
## set the matrix, get the matrix, set the inverse of a Matrix and get the inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                                                 ## inicialize the inv object, wich will cache the inverse of x
set <- function(y) {                                        ## set the matrix and reset inv
    x <<- y
    inv <<- NULL
}
get <- function() x                                         ## get the matrix
setinv <- function(inverse) inv <<- inverse                 ## set the inverse on the cache inv
getinv <- function() inv                                    ## get the cached inverse inv

list(set = set, get = get,setinv = setinv,getinv = getinv)  ## output list with the functions defined
}


## This function calculates the inv of a matrix using a list created with the function makeCacheMatrix as input

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                             ## Get the inverse cached and check if it was calculated
    if(!is.null(inv)) {
        message("getting cached data")            ## If inverse was calculated then return the value and print a message
        return(inv)
    }
    matr <- x$get()                               ## Else get the matrix
    inv <- solve(matr, ...)                       ## Calculate the inverse
    x$setinv(inv)                                 ## Cache and return the inverse
    inv
}
