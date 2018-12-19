## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#Default of argument x is an empty matrix.
    inv = NULL                              #Creates inv, an empty object to be used later.    
    set = function(y) {                     # function with argument y 
        x <<-y                              # y is assigned to x, the matrix in parent eniviroment of makeCacheMatrix. 
        inv <<- NULL                        # NULL is assigned to inv, also in parent enviroment.
    }
    get = function() x                      # x is not defined within function get(), therefore R retrive it from parent enivorment.
    setinver = function(inverse)            # setter function for the inverse.
        inv <<- inverse                     # inv is in parent enivorment.
    getinver = function() inv               # R retrive the value of inv from parent enivorment.
    list(set = set, 
         get = get, 
         setinver = setinver, 
getinver = getinver) # Finally, a list is returned.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
inv = x$getinv()                       # gets the inverse from x. 
    
    if (!is.null(inv)){                    # If the result is NULL..
        message("getting cached data")
        return(inv)                        # ...then return the value inv to parent enviroment.
    }
    
    data = x$get()                         # If !is.null is FALSE, the function get x.  
    inv = solve(data, ...)                 # And calucalte the inverse.
    x$setinv(inv)                          #     
return(inv) # Returns the inverse.        ## Return a matrix that is the inverse of 'x'
}
