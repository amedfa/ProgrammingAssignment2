## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix function  creates a special "matrix",
# which is really a list containing a function to
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of a matrix
# 4. get the value of the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        m_inver <- NULL
        set <- function(y) {
                x <<- y
                m_inver <<- NULL
        }
        get <- function() x
        setinver <- function(inver) m_inver <<- inver
        getinver <- function() m_inver
        list(set = set, get = get, setinver = setinver, getinver = getinver)
}

#The cacheSolve function calculates the Inverse of the
#special "Matrix" created with the makeCacheMatrix function.
#However, it first checks to see if the Inverse has already been 
#calculated. If so, it gets the Inverse from the cache and skips the computation.
#Otherwise, it calculates the Inverse of the data and sets the value of the Inverse 
#in the cache via the setinver function.


cacheSolve <- function(CacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x' in CacheMatrix
        m_inver <- CacheMatrix$getinver()
        if(!is.null(m_inver)) {
                message("getting cached data")
                return(m_inver)
        }else {
                message("setting cached data")      
        }
        data <- CacheMatrix$get()
        m_inver <- solve(data, ...)
        CacheMatrix$setinver(m_inver)
        m_inver
}

# Sample run:
# source("cachematrix.R")
# x<- matrix(c(3,1,2,1),nrow=2,ncol=2)
# CacheMatrix<-makeCacheMatrix(x)
# CacheMatrix$get() == x #
# cacheSolve(CacheMatrix)#1st
# cacheSolve(CacheMatrix)#2th
# cacheSolve(CacheMatrix)#3th

# Result
# source("cachematrix.R")
# > x<- matrix(c(3,1,2,1),nrow=2,ncol=2)
# > CacheMatrix<-makeCacheMatrix(x)
# > CacheMatrix$get() == x
# [,1] [,2]
# [1,] TRUE TRUE
# [2,] TRUE TRUE
# > cacheSolve(CacheMatrix)#1st
# setting cached data
# [,1] [,2]
# [1,]    1   -2
# [2,]   -1    3
# > cacheSolve(CacheMatrix)#2th
# getting cached data
# [,1] [,2]
# [1,]    1   -2
# [2,]   -1    3
# > cacheSolve(CacheMatrix)#3th
# getting cached data
# [,1] [,2]
# [1,]    1   -2
# [2,]   -1    3
# > 
