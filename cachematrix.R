makeCacheMatrix <- function(x = matrix())
{
    # This function creates a special "matrix", which is really
    # a list containing four functions F1 through F4
    i <- NULL
    set <- function(y)  # F1: to set the value of the matrix
    {
        x <<- y
        i <<- NULL  # initializes inverse to NULL while caching the matrix
    }
    get <- function() x # F2: to get the cached value of the matrix
    setinv <- function(solve) i <<- solve # F3: to set the value of the inverse
    getinv <- function() i # F4: to get the cached value of the inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    # to return the list of four functions (F1 through F4)
}

cacheSolve <- function(x, ...)
{
    # This funcion calculates the inverse of a special "matrix"
    # created with the function makeCacheMatrix()
    i <- x$getinv() # fetches the cached value of matrix inverse
    if(!is.null(i)) 
    {
        # if the fetched value is non-null => inverse was calculated before
        message("getting cached matrix inverse")
        return(i) # return the cached value of inverse
    }
    mat <- x$get() # if inverse is null, fetch the cached matrix 
    i <- solve(mat, ...) # evaluate the inverse of the cached matrix
    x$setinv(i) # cache the matrix inverse for future use
    i # return the matrix inverse
}
