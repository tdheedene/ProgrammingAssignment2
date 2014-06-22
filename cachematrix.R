## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix contains the logic to cache the 
## inverse of a matrix. It accepts a matrix (of which the inverse 
## needs to be cached) as an input , and returns a list of functions 
## to use the caching logic. This caching logic is specified in 4 
## other functions.
## These functions are:
## 1. set: define of what matrix(saved in x) the inverse should be cached
## 2. get: return the matrix of which the inverse should be cached
## 3. setinverse: save the calculated inverse value of the matrix (saved in cachedinverse)
##                --> cache the inverse (the value of the inverse is NOT calculated in this function)
## 4. getinverse: return the cached inverse of the matrix (if not calculated it returns NULL)

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  
  # define the set function
  # this function sets the data matrix of which the invers needs to be cached 
  # and resets the cached inverse value
  set <- function(y){
    x <<- y
    cachedinverse <<- NULL
  }
  
  #define the get function
  #this function returns the data matrix of which the inverse needs to be cached
  get <- function(){
    x
  }
  
  #define the setinverse function
  #this function sets the cached value of the inverse
  setinverse <- function(inverse){
    cachedinverse <<- inverse
  }
  
  #define the getinverse function
  #this function returns the cached value of the inverse
  getinverse <- function(){
    cachedinverse
  }
  
  #return the functions in a list so that they can be called
  list(
    set         = set         , 
    get         = get         , 
    setinverse  = setinverse  , 
    getinverse  = getinverse
  )
}


## Write a short comment describing this function
## The function cacheSolve is used to call the caching logic 
## (defined in the function makeCacheMatrix) in case it is needed.
## it accepts the output of the function makeCacheMatrix (specified above)
## as input and returns the inverse of the matrix (specified by calling 
## the set function of makeCacheMatrix). If it is the first time that the 
## inverse is requested for a certain matrix (the inverse has not been cached ), then 
## the inverse is calculated and cached using the functionality in makeCacheMatrix.
## if the function cacheSolve has been called before, then the cached value is returned
## instead of calculating the inverse again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting the cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
