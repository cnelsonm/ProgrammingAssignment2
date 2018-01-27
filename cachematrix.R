## Put comments here that give an overall description of what your
## functions do
### setmtx -> will create the square matrix
### getmtx -> will print the square matrix
### mkinv  -> will create the inverted
### getiv  -> will print the inverted

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        setmtx <- function(y) {
                x<<-y
                inv<<-NULL
        }
        getmtx <- function() x
        setinv <- function(solve) inv <<- solve 
        getinv <- function() inv
        list(semtx = setmtx, getmtx = getmtx,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
### This function will do the work inverting the matrix 
### and making it avaliable to others functions 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                return(inv)
        }
        data<-x$getmtx()
        inv <-solve(data,...)
        x$setinv(inv)
        inv
}