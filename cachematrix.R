## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {


                inv_matrix <- NULL  ## Initialises the inv_matrix to NULL         
                set_matrix <- function(y) {
                        x <<- y
                        inv_matrix <<- NULL
                }
                get_matrix <- function() {
                        x
                }
                set_inv <- function(inv) { 
                        inv_matrix <<- inv
                }
                get_inv <- function() {
                        inv_matrix
                }
                ## The return value of makeCacheMatrix function is a list of 4 functions
                list(set_matrix = set_matrix, 
                     get_matrix = get_matrix,
                     set_inv = set_inv,
                     get_inv = get_inv)
       
        
        
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv()
        
        if(is.null(inv)) {
                data <- x$get_matrix()
                inv <- solve(data, ...)
                x$set_inv(inv)
        }
        else{
        message("getting cached data")
        }
        return(inv) 
        ## return the inverse matrix
                
}
