## Functions to set a matrix and its inverse as cache and use the cache inverse
## repititively when needed

## Function to create a special matrix element to set cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
    I<-NULL
    
    set<-function(y){
        x<<-y
        I<<-NULL
    }
    
    get<-function() x
    
    setinverse<-function(inverse)I<<-inverse
    
    getinverse<-function() I
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to check whether the inverese is already available in cache
## and if not calculate and set the inverese

cacheSolve <- function(x, ...) {
        I<-x$getinverse()
        
        fi(!is.null(I)){
            message("getting cached data")
            return(I)
        }
    
        data<-x$get()
        I<-solve(data, ...)
        x$setinverse(I)
        I
    
}
