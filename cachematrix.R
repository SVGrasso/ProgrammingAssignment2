#
#this function sets up some empty objects and some functions that can be called by CacheSolve under the right conditions
makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  # i is NULL here so it has a default value so the code will work
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # the set function assigned the matrix to x and clears i of any existing value by assigning NULL to it
  get <- function() x 
  # get has a function that retrieves x, the matrix put into makeCacheMatrix
  setin <- function(solve) i <<- solve
  # setin has the function that will invert x if called
  getin <- function() i
  # getin has the function that calls i the inverted matrix
  list(set = set, get = get, 
        setin = setin, 
        getin = getin)
 # makeCacheMatrix returns a list of these functions, indexed by their names, that can be called by cacheSolve

}


# cacheSolve takes makeCacheMatrix as an argument, checks to see is an inverted matrix has been created
# and runs the functions from makeCacheMatrix to create an inverted matrix if one hasn't been created
cacheSolve <- function(x, ...) {
  i <- x$getin()
  # retrieves i from the list made by makeCacheMatrix
  if(!is.null(i)){
      message("getting cache data")
    return(i)
  }
  # if i is not Null then cacheSolve returns i along with the message "getting cache data"
  data <- x$get()
  # if the above if statment is not met the matrix to be inverted if retrieved and assigned to data
  i <- solve(data, ...)
  x$setin(i)
  i
  
}
