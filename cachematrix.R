#################################################################
### Assignment: Matrix Inversion
#################################################################
### Matrix inversion is usually a costly computation and there may be some benefit 
### to caching the inverse of a matrix rather than computing it repeatedly 
### (there are also alternatives to matrix inversion that we will not discuss here). 
### Your assignment is to write a pair of functions that cache the inverse of a matrix.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {    
        inv <- NULL
        
        get <- function() x
        getinverse <- function() inv
        
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        setinverse <- function(inverse) inv <<- inverse
        
        list(get = get, getinverse = getinverse, set = set, setinverse = setinverse)
}

# mat <- matrix(1:4, 2, 2)
# mat
# 
# res <- makeCacheMatrix(mat)
# res$get()
# res$getinverse()
# 
# mat1 <- matrix(4:7, 2, 2)
# mat1
# res$set(mat1)
# res$get()
# res$getinverse()
# 
# 
# mat2 <- matrix(6:9, 2, 2)
# mat2
# mat2inv <- solve(mat)
# mat2inv
# res$set(mat2)
# res$setinverse(mat2inv)
# res$get()
# res$getinverse()


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}



# Sample run:
# x <- rbind(c(1, -1/4), c(-1/4, 1))
# x
# m <- makeCacheMatrix(x)
# m$get()

# No cache in the first run
# cacheSolve(m)

# Retrieving from the cache in the second run
# cacheSolve(m)




#################################################################
### Example: Caching the Mean of a Vector
#################################################################
### In this example we introduce the `<<-` operator which can be used to
### assign a value to an object in an environment that is different from the
### current environment. Below are two functions that are used to create a
### special object that stores a numeric vector and caches its mean.


# The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }


# The following function calculates the mean of the special "vector"
# created with the above function. However, it first checks to see if the
# mean has already been calculated. If so, it `get`s the mean from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the `setmean`
# function.
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }


# x <- c(2,4,6,8)
# x
# m = makeVector(x)
# m$get()
# m$getmean()
# m$setmean(5)
# 
# cachemean(m)



