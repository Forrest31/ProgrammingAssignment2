# makeCacheMatrix is a set of functions that caches a set of inverse matrices
#The first object (set) creates a function that allows us to input a value (x)
#create a matrix.  The get object creates a blank function for x.  The setsolve
#object which is the output of the inverse matrix of m, which was set to null in
#the set #object.  The getsolve object is the output of a function performed on
#m.  Finally, the list command creates a list all 4 objects (set, get, setsolve,
#and getsolve).

## Write a short comment describing this function: see above

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachesolve looks for any value of m that has already been solved and labels
## it as "getting cached data" rather than solving again for it. If m has not
## been solved for, the function will solve for the value of m and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
