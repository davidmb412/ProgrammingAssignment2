

## ## Creates a creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the cache
#get the value of the cache



makeCacheMatrix <- function(x = matrix()) {
  
    cacheMatrix <- NULL
    
    
    
    set <- function(y) {
      x <<- y
      cacheMatrix <<- NULL
    }
    
    
    get <- function() x 
    
    
    setCache <- function(inverseMatrix) cacheMatrix <<- inverseMatrix
    
    
    getCache <- function() cacheMatrix
    
    
    
    list(set = set,
         get = get,
         setCache = setCache,
         getCache = getCache)
  }
  




## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
    cacheMatrix <- x$getCache()
    
    
    
    if (!is.null(cacheMatrix)) {
      message("getting cached matrix.")
      return(cacheMatrix)
    }
    
    
    
    dataMatrix <- x$get()
    cacheMatrix <- solve(dataMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
    
    
  }
