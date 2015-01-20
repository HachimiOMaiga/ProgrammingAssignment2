## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The following pair of functions
## compute and cache the inverse of a matrix.

## @parameter x: a matrix to be inversed
## @return list(set, setInverse, get, getInverse): list of funtions
## This function takes a matrix to be inversed as input and returns a list of function that
## can be applied to the matrix taken as input.

makeCacheMatrix <- function(x = matrix()) 
{
    # Initialises a variable that will hold the inverse of the input matrix
    inverse <- NULL
    
    ## Defines a setter for makeCacheMatrix
    ## It is assumed that the matrix will only be modified using this set function.
    ## Without this assumption one would need to add a check before computing the inverse.
    ## Therefore, when the matrix is updated/changed its inverse is also flushed 
    ## so that a new inverse is calculated.
    ## @parameter y: a matrix
    set <- function(y) 
    {
        ## Sets the matrix 
        x <<- y
        
        ## Flushes the inverse
        inverse <<- NULL
    }
    
    ## Defines a getter for makeCacheMatrix, i.e. allows to retrieve the matrix.
    get <- function() 
    {
        ## Returns the matrix
        x
    }
    
    ## Defines a setter for the inverse
    ## @parameter inverseParameter: a matrix
    setInverse <- function(inverseParameter) 
    {
        ## Sets the inverse
        inverse <<- inverseParameter
    }
    
    ## Defines a getter for the inverse
    getInverse <- function() 
    {
        ## Returns the inverse
        inverse
    }
    
    ## Returns this list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## @parameter x: returned object from a makeCacheMatrix 
## @return inverse: a matrix that is the inverse of 'x' passed in makeCacheMatrix
## This function takes a makeCacheMatrix object and checks whether the matrix passed to 
## makeCacheMatrix has been inversed or not. If yes then it retrieves the inverse from the cache,
## if no then computes the inverse and caches it.

cacheSolve <- function(x, ...) 
{
    ## Retrieves the inverse
    inverse <- x$getInverse()
    
    ## Checks if the inverse has already been computed for the matrix provided in makeCacheMatrix
    if(!is.null(inverse)) 
    {
        ## If the inverse was computed before then returns this message & returns the inverse
        message("getting cached data")
        return(inverse)
    }
    
    ## If the inverse was not computed before for the matrix provided in makeCacheMatrix
    ## Then gets the the matrix from the get of makeCacheMatrix
    data <- x$get()
    
    ## Inversees the matrix
    inverse <- solve(data, ...)
    
    ## Caches the inverse
    x$setInverse(inverse)
    
    ## Returns a matrix that is the inverse of 'x' provided in makeCacheMatrix
    inverse
}