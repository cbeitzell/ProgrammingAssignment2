## The following functions are modelled after the vector cache
## examples provided in the class.
## The purpose of the makeCacheMatrix is to take a matrix, and create 
## an object with a series of  functions to represent the matrix.  Of the functions
## there will be a set function to place the matrix and its inverse into memory of the
## parent environment.  
## This object is then taken by the cacheSolve to create the inverse matrix, but
## only if doesn't exists in the environment/memory.  If it does, then it is retrieved 
## and returned

## The makeCacheMatrix function can take a matrix as a variable,
## otherwise it will create a 1x1 matrix with elements NA. It contains four functions:
## set, get, setinverse, and getinverse.  
##
## The set function will run when the makeCacheMatrix is called.  Set takes a variable
## and sets it to the x (matrix) variable, and sets the initial m(inverse matrix) variable to NULL.
## Both of variables use the <<- assign operators to store the values to the parent environment.
##
## The get function will return the original matrix stored in the variable x.
##
## Setinverse takes the variable solve, which is the inversed matrix of the matrix x.  In this case solve can
## be a slightly confusing variable name as it is also the name of the function to calculate the inverse 
## matrix.  R allows this type of ambiguity.  The value in solve is assigned to the variable m created in 
## the parent environment.
##
## The getinverse function will return the value of the variable m from parent environment.
##
## When the makeCacheMatrix function is called, the local functions are initialized to create the object.  
## Set does not need to be called explicitly as it runs as part of the creation of the makeCacheMatrix 
## function.
##
## The last thing the function does is to create a list from the environment.  It creates the name-pair to 
## values, so that objects like set and get can be  called outside of the created environment.
## 
makeCacheMatrix <- function(x = matrix()) {
        ## X is the matrix to be converted, it is assumed to be a square; no need to verify.
        ## m is the inverse matrix, initally set to NULL
        m <- NULL
        
        ## set the initial matrix to the parent environment.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## the get function retrieves the matrix.
        get <- function() x
        
        ## Set the inversed matrix solve to the cached variable m.  
        setinverse <- function(solve) m <<- solve
        
        ## the getinverse function returns the inversed matrix
        getinverse <- function() m
        
        ## set the names to values in the environment list.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a matrix created by the makeCacheMatrix function.  It returns the 
## inverse of the matrix.  It takes the variable x, which is the matrix object.  It creates a local variable m 
## to represent the inversed matrix, and assigns it a value using the getinverse function of the matrix 
## object.  After being set, it checks to see if m is NULL.  If it isn't, then it returns the value.
##
## If m is NULL, then it creates a local variable data and using the get function of the matrix object, the
## value of the matrix is assigned.  The matrix value (data) is then used with the solve function to assign 
## the inverse matrix to the variable m.
##
## m is then sent to the setinverse function of the matrix object to be set in the parent environment, so
## that it can be recalled if needed again.  Lastly the variable m is returned so that the function can
## display the calculated inversed matrix.

cacheSolve <- function(x, ...) {
        ## x is the matrix object
        ## m is the local inversed matrix
        
        ## the m matrix is set to the getinverse function of the x matrix object to get
        ## the saved invered matrix.
        m <- x$getinverse()
        
        ## if the matrix is not NULL the print out a
        ## get cache message, and return the inveresed
        ## matrix.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the matrix is NULL, it hasn't been set.
        ## In which case it runs the get function of the
        ## x matrix object to get the original matrix.
        data <- x$get()
        
        ## the solve function is run on the original matrix
        ## to give the inversed matrix and assign it to m.
        m <- solve(data, ...)
        
        ## the inversed matrix is then set to the x matrix object
        ## using the setinverse function.
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        return(m)
}
