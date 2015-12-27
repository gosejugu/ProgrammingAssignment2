## The pair of functions create a square invertible matrix 
## (makeCacheMatrix function) and make the inverse of the matrix 
## available in the cache environment (cacheSolve function)
##


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
     	set <- function(y) { 
        x <<- y 
        inv <<- NULL 
     } 
    get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

inv <- x$getinverse() 
   if(!is.null(inv)) { 
        message("getting cached data.") 
         return(inv) 
    } 
    data <- x$get() 
    inv <- solve(data) 
     x$setinverse(inv) 
    inv 
 } 


## test
## 
## > source("cachematrix.R")    load cachematrix.R program
## > a <- makeCacheMatrix()     create cachematrix function
## > a$set(matrix(1:4, 2, 2))   create matrix in working environment
## > cacheSolve(a)              first time you run it returns inverted matrix
##                              
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              the second time you run it returns
##                              inverted matrix from cache
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

