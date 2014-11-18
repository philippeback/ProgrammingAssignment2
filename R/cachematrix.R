# Note: Documentation has been written using RDoc.
# install.packages("devtools" "roxygen2")
# library(devtools)
# library(doxygen2)
# put the R file in a R/ directory
# write a DESCRIPTION file
# run devtools:document()
# Then you can use 
# ?makematrix to read the help

#' Provides support for matrix inversion caching.
#' 
#' @param x A reference matrix
#' @return The list of closures (set, get, setinverse, getinverse) giving access to the matrix and
#'   its cached inverse. 
#' @examples
#'
#'  amatrix<-makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#'  amatrix$get()
#'  amatrix$set(matrix(c(5,6,7,8), nrow=2, ncol=2))
#'  amatrix$setinverse<-solve(m)
#'  i<-amatrix$getinverse()
#'  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #' Enables one to change the reference matrix.
  #' @param y A new reference matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #' Enables one to retrieve the stored reference matrix.
  #' @return The stored reference matric
  get <- function() x
  
  #' Enables one to set the inverted matrix for caching.
  #' @param inverse The calculated inverted matrix
  setinverse <- function(inverse) m <<- inverse
  
  #' Enables one to retrieve the inverted matrix that was cached.
  #' @return The cached inverted matrix of NULL if the inverted matrix has not been stored yet.
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#' Provides a cached varsion of the matrix inversion.
#' @param x a list coming out of makeCacheMatrix
#' @return The inverted matrix referenced by x. 
#' @examples
#' 
#' amatrix<-makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#' cacheSolve(amatrix) 
#' # first time, gets computed
#' amatrix$getinverse() 
#' # second time, get cached version
#' amatrix$getinverse()
#' 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
