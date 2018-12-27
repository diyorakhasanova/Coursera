# this function creates a special matrix object that caches its inverse

makecachematrix=function(x = matrix()) {
  inv=NULL
  set=function(y) {
    x <<- y
    inv <<- NULL
  }
  get=function() x
  setinv=function(inverse) inv <<- inverse
  getinv=function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}	 

# this function in turn calculates the inverse of the special matrix created by makecachematrix. In case the inverse was previously calculated, it should retrieve the inverse from the cache 

cacheSolve=function(x, ...) {
  inv=x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data=x$get()
  inv=solve(data, ...)
  x$setinv(inv)
  inv
}	 }