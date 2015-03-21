#NOTE: function description has been provided when they are defined. 

#makeCacheMatrix accepts only numeric argument, hence the x=numeric() part
makeCacheMatrix <- function(x = numeric()) {
      
      # initially nothing is cached, so the value of cache is NULL. 
      # In cacheInverse function, we assign the value of Inverse to the cache
      cache <- NULL
      
      # newMat is the value of matrix whose inverse is wanted. 
      # This function assigns value of newMat to x (stored matrix) and cache as NULL. 
      setMatrix <- function(newMat) {
            x <<- newMat
            # since the matrix is assigned a new value, cache is flushed. 
            cache <<- NULL
      }
      
      # returns the stored matrix
      getMatrix <- function() {
            x
      }
      
      # cache the given argument 
      cacheInverse <- function(solve) {
            cache <<- solve
      }
      
      # get the cached value
      getInverse <- function() {
            cache
      }
      
      # return a list. Each named element of the list is a function
      list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


#this function calls appropriate functions to perform 
#1) Storing matrix value
#2) retreiving matrix
#3) getting inverse and updating cache
#4) getting value from cache

cacheSolve <- function(y, ...) {
      # get the cached value
      inverse <- y$getInverse()
      
      # if a cached value exists return it
      #EXECUTED ONLY WHEN CACHE <> NULL
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # otherwise get the matrix, caclulate the inverse and store it in
      # the cache
      #EXECUTED ONLY WHEN CACHE =NULL
      else
      {
            data <- y$getMatrix()
            inverse <- solve(data)
            y$cacheInverse(inverse)
            
            # return the inverse
            return(inverse)
      }
      
}


