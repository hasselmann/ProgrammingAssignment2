## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates the matrix object corresponding to
## the matrix x

makeCacheMatrix <- function(x = matrix()) {

    ## m will later store matrix inverse,
    ## but initially is empty:
    m<-NULL

    ## set is not really required
    ## allows to set value of matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## get just returns the matrix
    get <- function() x

    ## setinv allows to set inverse of matrix
    setinv <- function(inv) m <<- inv

    ## getinv returns m as the inverse matrix 
    getinv <- function() m
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve calculates the inverse of the MATRIX OBJECT x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<- x$getinv()

    ## If value of m is defined (i.e. the inverse has
    ## been calculated previously) return m as the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## IF m=NULL: calculate the inverse using solve()
    data <- x$get()
    m<-solve(data,...)
    x$setinv(m)
    m
}
