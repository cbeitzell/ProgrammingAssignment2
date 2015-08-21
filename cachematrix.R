## The following functions are modelled after the verctor cache
## examples proveded in the class.
## The purpose of the makeCacheMatrix is to take a matrix, and create 
## an object with set and get functions to represent the matrix.  This object
## taken by the cacheSolve to create the inverse matrix if doesn't exists
## in the matrix object.

## Write a short comment describing this function

## The makeCacheMatrix function can take a matrix as a variable,
## otherwise it will create an emptry matrix. It creates four functions,
## a set, get,setinverse, and getinverse.  When the makeCacheMatrix function
## is run the local functions run createing the object.
makeCacheMatrix <- function(x = matrix()) {
        ## X is the matrix to be converted
        ## m is the inverse matrix, initally set to NULL
        m <- NULL
        
        ## the set function will run when the 
        ## makeCacheMatrix is called.
        ## the set function takes a variable
        ## and sets it to the x (matrix) variable.
        ## the m(matrix) variable is set to NULL.
        ## Both of te variables use the <<- assign operators
        ## to set the parent environment.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## the get function retrieved the matrix.
        get <- function() x
        
        ## the setinverse 
        setinverse <- function(solve) m <<- solve
        
        ## the getinverse function returns the inversed matrix
        getinverse <- function() m
        
        ## the list function will print out the functions listed
        ## the environment memory location the object.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a matrix created by the 
## makeCacheMatrix function.  It returns the inverse of the 
## the matrix.

cacheSolve <- function(x, ...) {
        ## x is the matrix object
        ## m is the local inversed matrix
        
        ## the m matrix is set to the getinverse 
        ## function of the x matrix object to get
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
