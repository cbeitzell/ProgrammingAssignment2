## The following functions are modelled after the vector cache  examples provided in the class.
##
## The purpose of the makeCacheMatrix  function is to take a matrix, and create 
## an object with a series of  functions to represent the matrix.  Within the function
## will be set functions to place the matrix and its inverse into memory of the
## parent environment.   In addition there will be get functions to retrieve the data.
## The second function is called cacheSolve.  It will take the object created by the makeCacheMatrix
## function and return an inverse of the matrix is found in the parent environment.  If the inverse matrix
## doesn't exist, it will calculate, set and return the inverse.

## The makeCacheMatrix function can take a matrix as a variable,
## otherwise it will create a 1x1 matrix with elements NA. It contains four functions:
## set, get, setinverse, and getinverse.  
##
## When the makeCacheMatrix is ran, the set will take a matrix variable and assign it to the x (matrix) 
## variable, and then assign the initial m(inverse matrix) variable to NULL.
## Both of variables use the <<- assign operators to store the values to the parent environment for the 
## purposes of caching.
##
## The get function will return the original matrix stored in the variable x.
##
## Setinverse takes the variable inverseMatrix, which should be the inversed matrix of the matrix x.  
## The value in inverseMatrix is assigned to the variable m in the parent environment.
##
## The getinverse function will return the value of the variable m from parent environment.
##
## When the makeCacheMatrix function is ran, the local functions are initialized to create the object.  
## Set does not need to be called explicitly as it runs as part of the creation of the makeCacheMatrix 
## function.
##
## The last thing the function does is to create a list from the environment.  It creates a name-pair to 
## values relationship, so that objects like set and get can be used outside of the local environment.
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
        setinverse <- function(inverseMatrix) m <<- inverseMatrix
        
        ## the getinverse function returns the inversed matrix
        getinverse <- function() m
        
        ## set the names to values in the environment list.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a matrix object  created by the makeCacheMatrix function.  It returns 
## the inverse of the matrix found within the object.  Taking the variable x, which is the matrix object, it 
## creates a local variable m  to represent the inversed matrix, and assigns it a value using the 
## getinverse function of the matrix object.  After being set, it checks to see if m is NULL.  If it isn't, then 
## it returns the value set in m.
##
## If m is NULL, then it creates a local variable data and using the get function of the matrix object to get 
## the matrix stored in the parent enviroment, the value of the matrix is assigned locally.  The retrieved  
## matrix value (data) is then used with the solve function to calculate the inversed matrix and assign 
## the value  to the local variable m.
##
## m is then sent to the setinverse function of the matrix object, so that it can be stored in the parent  
## environment to be recalled later if needed again.  Lastly the variable m is returned so that the 
## function can display the calculated inversed matrix.

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
