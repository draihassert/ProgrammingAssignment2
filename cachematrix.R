## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

##  This function creates a special "matrix" object that can cache its inverse
 makeCacheMatrix <-
        function(x = matrix()){
    m<-NULL
    set<-function(y) {
        x<<-y; m<<-NULL
    }
    get<-function() x
    setmatrix<-function(invm) m<<- invm
    getmatrix<-function() m
    list(set=set, get=get,
    setmatrix=setmatrix, getmatrix=getmatrix)
   }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m))
    {
        message("Getting inversed matrix from cache...")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
