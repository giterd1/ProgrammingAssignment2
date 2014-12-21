## The following pair of functions cache the inverse of a matrix 
## If a new matrix is supplied then its inverse will be calculated... 
##   ...and stored in the cache variable for subsequent uses

## Huge Thank You to all my mates in the forums and especially to... 
##    ...Greg Horne, Bill Hilton and Pavel Kirjanas for your posts
## Would have been impossible without all your help and encouragement!


## The first function makeCacheMatrix() is passed a matrix... 
##    ...which initializes the cache variable to NULL and creates...
##    ...a list of four functions - two that get (or read)... 
##    ...the values of the matrices and two that set (or change) them

  ## The last three functions in the list are defined but not run when... 
  ##    ...makeCacheMatrix() is called. These functions are called from...
  ##    ...the second function cachSolve()to get or set values of the inverse


makeCacheMatrix <- function(x = matrix()) {     # input 'x' will be a matrix
  
  i <- NULL           # 'i' will store the inverse of the matrix and...
                      #    ...it is reset to NULL every time makeCacheMatrix() is called
  
  set <- function(y) {        # Takes an input matrix,
                              #   ...is defined and called when makeCacheMatrix() is called
 
    x <<- y                   # Saves the input matrix
    i <<- NULL                # Resets the inverse to NULL,
   }                          #   ...basically what happens when a new object is generated
  
  
  get <- function() {x}       # This function returns the value of the original matrix
  
  setinv <- function(inv) {  i <<- inv }    # This is called by cacheSolve only if...
                                            #   ...i is NULL (on the first access) and...
                                            #   ...will set i, which is in the parent frame...
                                            #   ...using superassignment(<<-)
  
  getinv <- function() {  i  }              # This will return the cached matrix (i)...
                                            #   ...to cacheSolve() on subsequent accesses
  
  list(set = set,                     # Each time a new object is created, 
       get = get,                     #   ...i.e. each time makeCacheMatrix() is called,
       setinv = setinv,               #   ...a list of internal functions is created so that... 
       getinv = getinv)               #   ...the calling function knows how to access them
  
  }


## The second function cacheSolve() takes a matrix (x) as an argument...
##  ...and returns the inverse of x.

  ## It does so by first checking to see if the inverse has already...
  ##  ...been calculated and stored in cache memory,
  ##  ...returning the inverse from the cache if it has, 
  ##  ...and only goes on to calculate (solve) the inverse if it has not 

cacheSolve <- function(x, ...) {      # The input x is an object created by makeCacheMatrix()
        
  i <- x$getinv()                     # Access the object 'x' and get the value of the inverse
  
  if(!is.null(i))                     # If the inverse is NOT NULL, 
  {                                   #   ...i.e. if it has been cached during a previous run...
    message("getting cached data")    #   ...sends a message to the console,
    return(i)                         #   ...and returns the inverse, 
  }                                   #   ...thereby ending the function cacheSolve()
  
  data <- x$get()                     # We get to these parts only if x$getinv() returned NULL
                                      # In this case, the matrix 'data' is created by calling...
                                      #   ...x$get() which simply returns the orignial matrix
  
  i <- solve(data)                    # If i was NULL then we calculate the inverse of the matrix...
                                      #   ...by applying the solve() function to the matrix
  
  x$setinv(i)                         # Set or store the calculated inverse matrix in x...
                                      #   ...for further use by calling the setinv() function
  
  i                                   # And finally, return the inverse matrix to...
                                      #   ...the code that called this function
 }