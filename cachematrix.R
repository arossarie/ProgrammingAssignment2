## The functions below allow for caching the inverse of a matrix and getting it without having to recompute it everytime.

## This function takes a matrix as argument and creates a list of attribute and functions
## for getting and setting said matrix and its inverse  (so, "object-like" in a certain manner, what do you think ?)
makeCacheMatrix <- function(x = matrix())
{
        inverse <- NULL
        set <- function(y)
        {
                x <<- y
                Inv <<- NULL
        }
        get <- function(){x}
        setInverse <- function(inv) {inverse <- inv}
        getInverse <- function() {inverse}
}

## This function takes a matrix as first mandatory argument and returns the corresponding inverse matrix
## If the inverse is already in cache then it just returns it (hence skips its computation)
## else it calculates the inverse (using additionnal optional arguments in 'solve()'), caches it and finally returns it.
cacheSolve <- function(x, ...)
{
        inverse <- x$getInverse()
        if(!is.null(inverse))
        {
                message("getting cached data")
                return inverse
        }
        else ## I know it is not necessary because of the 'return' above, but I prefer it that way.
        {
                data <- x$get()
                inverse <- solve(data, ...)
                x$setInverse(inverse)
                return inverse
        }
}
