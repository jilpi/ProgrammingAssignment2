# makeCacheMatrix: Creates a list object, that is able to cache and return the inverse of a given Matrix.
# cacheSolve: Solves (ie. returns the inverse) of a given CacheMatrix object and cache it. If the inverse has already been calculated before, returns it from the cache.
#
# Comments and documentation format following guidelines from here: https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#functiondocumentation


makeCacheMatrix <- function(x = numeric()) {
  # Creates a list object containing 4 functions, that is able to cache and return the inverse of a given Matrix.
  #
  # Args:
  #   x: A square, invertible matrix
  #
  # Returns:
  #   A list of 4 functions (set, get, setinverse, getinverse).
  #
  # Note:
  #   The original matrix and its cached inverse are stored in the environment of the list object.
  
  # Initialize the cached inverse
  cached_inverse <- NULL
  
  set <- function(y) {
    # Save the matrix in argument in the parent's environment, and reinitialize the cached inverse to NULL.
    #
    # Args:
    #   y: A square, invertible matrix
    #
    # Returns:
    #   nil
    #
    # Variables from parent environment:
    #   x: the matrix itself
    #   cached_inverse: the cached inverse of the matrix
    x <<- y
    cached_inverse <<- NULL
  }
  
  
  get <- function() {
    # Returns the matrix x stored in the parent environment.
    #
    # Args:
    #   nil
    #
    # Returns:
    #   the matrix x
    #
    # Variables from parent environment:
    #   x: the matrix itself
    x
  }
  
  
  setinverse <- function(inverse) {
    # Set the cached inverse of the matrix in the parent environment.
    #
    # Args:
    #   inverse: the inverse of the matrix to be cached
    #
    # Returns:
    #   nil
    #
    # Variables from parent environment:
    #   cached_inverse: the cached inverse of the matrix
    cached_inverse <<- inverse
  }
  
  getinverse <- function() {
    # Returns the cached inverse of the matrix matrix x stored in the parent environment.
    #
    # Args:
    #   nil
    #
    # Returns:
    #   the cached inverse of the matrix
    #
    # Variables from parent environment:
    #   cached_inverse: the cached inverse of the matrix
    cached_inverse
  }
  
  # Returns the list of functions defining the "cacheMatrix" object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(cm, ...) {
  # Solves (ie. returns the inverse) of a given CacheMatrix object and cache it. If the inverse has already been calculated before, returns it from the cache.
  #
  # Args:
  #   cm: A "CacheMatrix" object
  #
  # Returns:
  #   The inverse of the matrix stored in the CacheMatrix object.
  
  # Get the cached inverse (NULL if the inverse was never calculated before)
  inverse <- cm$getinverse()
  
  # If the cache was set (not NULL)
  if(!is.null(inverse)) {
    # Returns the cached inverse and exit the function
    message("getting cached data")
    return(inverse)
  } else {
    # Caculates the inverse
    inverse <- solve(cm$get(), ...)
    # Store it in the cache
    cm$setinverse(inverse)
    # Returns the inverse and exit the function
    return(inverse)
  }
}

