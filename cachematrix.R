## Program requires proper matrix/data. 
## To test the program make invertable matrix, e.g.,  C <- matrix(1:4,2,2) 
## Load makeCacheMatrix.R program file which contains both functions
## makeCacheMatrix() and cacheSolve() from your getwd() directory or 
## other absolute path name dpecified directory
##   Run: 
##       listM <- makeCacheMatrix (C)
##       cacheSolve (listM)
##  Test:
##      C %*% cacheSolve(listM)
##  You shold get unit matrix
##  To see effects of caching you should have large memory and try to run the following test
##   	D <- matrix(round(rnorm(1000000,0,100),digits=0),1000,1000)
##	listM <- makeCacheMatrix (D)
##	cacheSolve (listM)
##
##  First function makeCacheMatrix() accepts matrix x and creates a list of functions applicable  
##     on x  matrix (It appears as a class object in Java or C++. It has data and functions/methods)
##     The most important function is setsolve() which inverts matrix via solve() function 
##
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL		
        set <- function(y) {	
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve)  s <<- solve   ## Has to work on the data of the caller
        getsolve <- function() s
        list(set = set, get = get,  setsolve= setsolve, getsolve = getsolve)
}

## Function cacheSolve() is paired/strongly-copupled with makeCacheMatrix() function.
##   Mem-cache is very sensitive and easy become unusable, i.e., s becomes NULL
##   This feature is used by cacheSolve() to decide to use cache or not. 
##   Function cacheSolve() is using list x of functions provided by makeCacheMatrix()
##   
cacheSolve <- function(x, ...) {
        s <- x$getsolve()	## The list x with data matrix is used here 
        if(!is.null(s)) {
                message("Getting cached data")
                return(s)
        }
        data <- x$get()		## The list x is used here to fetch matrix data
        s <- solve(data, ...)	## Matrix data is inverted into matrix s
        x$setsolve(s)    	## The list x is used here again 
        s
}
