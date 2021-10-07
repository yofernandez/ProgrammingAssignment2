## This is a pair of functions that cache the inverse of a matrix,
## given that the matrix is invertible. 

## This first function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## Assuming that the matrix is invertible

        inv <- NULL
      
    ## To set the value of the matrix
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
    ## To get the value of the matrix  
        
        get <- function() {x}
        
    ## To set the value of the inverse
        
        setInv <- function(inverse) {inv <<- inverse}
        
    ## To get the value of the inverse
        
        getInv <- function() {inv}
      
      list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## This second function computes the inverse of the matrix returned by the
## initial matrix above.

cacheSolve <- function(x, ...) {
    
    ## Returns a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
    
    ## To check if the inverse has been calculated
        
        if(!is.null(inv)){
              message("gets cached data")
              return(inv)
        }
    
    ## Otherwise, to compute the inverse of the matrix
        
        mat <- x$get()
        inv <- solve(mat, ...)
        
    ## To set the value of the inverse
        
        x$setInv(inv)
        inv
}

## End of script