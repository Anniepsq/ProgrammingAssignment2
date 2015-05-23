## makeCacheMatrix is a function that stores a list of functions, including set,get, setinverse and getinverse.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        ## set the value of the matrix
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        ## get the value of the matrix
        get<-function() x
        ## set the value of the inverse
        setinverse<-function(inverse) m<<-inverse
        ## get the value of the inverse
        getinverse<-function() m
        ## the following line stores the four functions
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m<-x$getinverse()  ## Return a matrix that is the inverse of 'x'
        if (!is.null(m)){
                message("getting cached data")
                return(m) ## if the inverse has been calculated before, the message show up and the inverse is given
        }
        ## if the inverse has not been calculated, then call the matrix, claculate its inverse and store its inverse
        data<-x$get() 
        m<-solve(data,...) ##calculate the inverse using solve function
        x$setinverse(m)
        m
}

## the following is an example for testing
x <- makeCacheMatrix(matrix(1:4, 2,2))

