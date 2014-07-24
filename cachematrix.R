## Calculating and caching the inverse of a invertible matrix
## It will retrieve the previous matrix inverse if the matrix input has not changed

## This function will:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y              # this set a new input
                m <<- NULL           # ... then reset any cached matrix inverse
        }
        get <- function() x          # get 'x'
        setinverse <- function(inverse) m <<- inverse # set the existing inverse from the parent environment
        getinverse <- function() m   # search the existing inverse (cache)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will:
## 1. print a message if the cached inverse exists (the matrix input has not changed)
## 2. solve the inverse if the new matrix input found
## 3. store the inverse (cache)
## 4. return the inverse of the new matrix input

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                      # try to get the cached inverse
        if(!is.null(m)) {                        # if this inverse exists
                message("getting cached data")   # ... then pop the message
                return(m)                        # return the cache
        }
        data <- x$get()  # get matrix from get() defined in makeCacheMatrix()
        m <- solve(data, ...) # calculate the matrix inverse
        x$setinverse(m)       # cache the matrix inverse for the later use
        m                     # Return a matrix that is the inverse of 'x'
}
