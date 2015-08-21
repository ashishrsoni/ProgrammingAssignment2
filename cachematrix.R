## writing a pair of functions that cache the inverse of a matrix as per Assignment 2
## Wrote the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed),
##              then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix():This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
        ## @a: an square invertible matrix
        ## return: a list containing functions to
        ##              1. setting the matrix
        ##              2. gettiing the matrix
        ##              3. settting the inverse
        ##              4. getting the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(b) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                a <<- b
                inv <<- NULL
        }
        get = function() a
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(c, ...) {
        ## @c: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = c$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = c$get()
        inv = solve(mat.data, ...)
        
        # setting the value of the inverse in the cache via the setinv function.
        c$setinv(inv)
        
        return(inv)
}
