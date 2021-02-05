
## Here we set a list of functions and return them in a matrix to the parent
## environment, we will use them later. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                  #we create an empty place to store the data
    set <- function(y){        
        x <<- y
        m <<- NULL
    }
    get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set,              #gives the name set to the set() function
        get = get,                   #defined above, we repeat with the other
        setsolve = setsolve,         #functions. This helps us later to use "$".
        getsolve = getsolve)     
}

## In this function we actually get the inverse of the matrix. First we check
## if the matrix is stored in cache data, if it's not, then we calculate the
## inverse using the functions from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")  #if the data is stored, inform it and
        return(m)                       #return the value cached.
    }
    data <- x$get()                     #if the data is not stored, calculate 
    m <- solve(data,...)                #the inverse of the matrix.
    m
}