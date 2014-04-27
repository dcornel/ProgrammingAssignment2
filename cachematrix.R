## The bellow functions give the posibility of storing the inverse of a 
## matrix in cached memory. This way the inverse is calculated the first time,
## stored, and for the same matrix, the subsequent times inverse is called the
## value is taken from cache, thus avoiding the need to recalculate it.


## makeCacheMatrix function creates a special matrix object, by giving the options of
## of reading and writting(in cache) a matrix passed as arguement, and saving 
## in chache and getting the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        
        set <- function (y)
        {
                x<<- y 
                minverse <<- NULL 
        }
        
        get <- function () x 
        
        setInverse <- function(solve) minverse<<- solve #store in cache
        getInverse <- function() minverse 
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
}


#cacheSolve - computes the inverse of matrix object created with makeCacheMatrix.
#if the inverse of the matrix has already been calculated, the value stored in 
#cache is returned. If not the inverse is calculated and the value is stored in cache.

cacheSolve <- function(x, ...) {
        minverse <- x$getInverse()
        
        if(!is.null(minverse)) #if it has been computed before and stored in cache
        {
                message("getting cached data")
                return(minverse) #return the value stored in cache. exit the function
        }
        # else (if minverse is null)
        data<- x$get() 
        minverse <- solve(data,...) #compute matrix inverse using solve R function
        x$setInverse(minverse) #store in cache
        minverse #return a matrix that is the inverse of 'x'
        
}
