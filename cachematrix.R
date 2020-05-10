## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix creates a matrix object that can cache its own inverse.
# x holds the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){       # set function assigns new value of the matrix in parent environment.
                x<<-y
                i<<-NULL        # inverse i must be reset to null
                }
        get<-function()x
        setInverse<-function(inverse)i<<-inverse        # appoints value of inverse in parent environment.
        getInverse<-function()i                         # returns inverse of the matrix.
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
# The function cacheSolve computes the inverse of the matrix as returned by the function makeCacheMatrix.
# If the inverse is already calculated for the matrix then, the inverse is retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInverse()
        if(!is.null(i)){
                # Cache data being received.
                return(i)
                }
        mat<-x$get()
        i<-solve(mat,...)
        x$setInverse(i)
        i
}
