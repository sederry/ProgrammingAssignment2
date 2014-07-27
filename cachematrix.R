##This task is divided by 2 parts. First one (MakeCacheMatrix) is writing the inverse to cache memory and second
##(cachesolve) is looking for it and if it is found it is pulling answer from cache memory. Else it is computed 
##and stored in memory

makeCacheMatrix <- function(x = matrix()){                     # defining the function
  inverse <- NULL                                              # initializing the inverse variable
  set <- function(y){                                          # setting 'set' as a function to store the value of matrix
    x <<- y                                                    # initiate x and set it to be = y
    inverse <<- NULL                                           # initiate inverse and sets it to NULL
  }  
  get <- function() x                                          # obtaining the value of the matrix  and assign it to `get` value
  setinv <- function(solve)                                    # the `setinv` function uses the inbuilt function 'solve' to determine the inverse of the matrix x
  inverse <<- solve                                            # assign `solve` function it inv value
  getinv <- function() inverse                                 # the `getinv` function is used to obtain value of the inverse function of the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) # set the list
}


## This part is looking for matrix inverse in memory. If it is found, then the value is pulled out from the cache,
## else it is computed and stored in the memory


cacheSolve <- function(x, ...){     # defining the function, assigning it it CacheSolve
    m <- x$getinv()                 # looking for the inverse of the function in different environment and assignt it to m
    if (!is.null(m)){               # if the value is found
    message("found in cache")       # print the message
    return(m)                       # and return the value
  }
  else{                             # if not
  data <- x$get()                   # the inverse of the matrix is computed using the solve function from previous part
  m <- solve(data,...)              # assigning the inverse of the matrix to the m
  x$setinv(m)                       # setting inverse to be x
  m
  return(m)                         # and returning the answer
}
}