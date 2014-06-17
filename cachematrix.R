# =============================================================
# Author:      Michael O'Flaherty (michael@oflaherty.com)
# Create date: 6/15/2014
#
# Description: Created from a fork of 
#              https://github.com/rdpeng/ProgrammingAssignment2
#
#              The purpose of this assignment is to create a pair
#              of functions that cache the inverse of a matrix so
#              subsequent calls pull from the cache (for speed)
#              instead of calculating it every time.
# =============================================================

# ================================================================
# Function:   makeCacheMatrix
# Summary:    This function creates a special "matrix" object that 
#             stores the matrix and appends functions to it.
# Param:      x: The matrix to store.
# Interface:  set: Updates the value and clears the cache.
#             get: Returns the stored value.
#             setinverse: Stores the inverted object.
#             getinverse: Returns the inverted object.
# ================================================================
makeCacheMatrix <- function(x = matrix()) {
  
  # seed the storage object m to NULL; it will be used below
  m <- NULL  
  
  # define the set function; this function will populate the x 
  # variable and set the m object to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # define a get function which returns x
  get <- function() x
  
  # define the setinverse function that will store the inverse
  setinverse <- function(inverse) m <<- inverse
  
  # define the getinverse function that returns the m object
  getinverse <- function() m
  
  # build and return a list of the functions we have defined.
  # this will be appended to the object (in this case, the
  # matrix.)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# ================================================================
# Function:   cacheSolve
# Summary:    This function computes the inverse of the special 
#             "matrix" returned by makeCacheMatrix function. If the 
#             inverse has already been calculated (and the matrix 
#             has not changed), then cacheSolve will retrieve the 
#             inverse from the cache.
# Param:      x; the special "matrix" container created with 
#             makeCacheMatrix
# ================================================================
cacheSolve <- function(x, ...) {
  
  # retrieve the inverse if it exists
  m <- x$getinverse()
  if(!is.null(m)) {
    
    message("getting cached data") # debug message; can be removed
    
    # found in cache; use this one
    return(m)
  }
  
  # we didn't find it; return the matrix
  data <- x$get()
  
  # run the solve function on it to invert
  m <- solve(data, ...)
  
  # store this so it is cached for subsequent calls
  x$setinverse(m)
  
  # return the value
  m
}

# ================================================================
# ========================  DEBUG SECTION  =======================
# ================================================================

# test code provided by Frank Evans here: https://class.coursera.org/rprog-004/forum/thread?thread_id=763
# testrand <- matrix(data=rnorm(9), nrow=3, ncol=3)
# testrand
# solve(testrand)
# testrand2 <- makeCacheMatrix(testrand)
# cacheSolve(testrand2)
# testrand2$get()
# testrand2$getinv()
# identical(cacheSolve(testrand2), solve(testrand))
# identical(testrand2$getinv(), solve(testrand))
# identical(testrand, testrand2$get())

# testbig <- matrix(data=rnorm(10000), nrow=100, ncol=100)
# testbig
# solve(testbig)
# testbig_c <- makeCacheMatrix(testbig)
# cacheSolve(testbig_c)
# identical(cacheSolve(testbig_c), solve(testbig))
# identical(testbig_c$getinv(), solve(testbig))
# identical(testbig, testbig_c$get())

# ================================================================
# ======================  END DEBUG SECTION  =====================
# ================================================================
