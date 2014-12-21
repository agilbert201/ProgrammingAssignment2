## Note I don't yet have enough knowledge of R or testthat
## to test caching logic more elegantly than with message checks
test_that("cacheMatrix returns accurate inverse",
{
    m1 <- matrix(c(1,2,3,4),2,2)
    m1inv <- solve(m1)
    cm <- makeCacheMatrix(m1)
    expect_message(inv <- cacheSolve(cm), "solving and caching new inverse")
    expect_identical(inv, m1inv)
    ## run again to ensure still this way from cache
    expect_message(inv <- cacheSolve(cm), "getting cached data")
    expect_identical(inv, m1inv)
})

test_that("cacheMatrix caches all n+1 queries",
{
    m1 <- matrix(c(1,2,3,4),2,2)
    cm <- makeCacheMatrix(m1)
    expect_message(cacheSolve(cm), "solving and caching new inverse")
    expect_message(cacheSolve(cm), "getting cached data")
})

test_that("cacheMatrix matrix change resets cache",
{
    m1 <- matrix(c(1,2,3,4),2,2)
    cm <- makeCacheMatrix(m1)
    expect_message(cacheSolve(cm), "solving and caching new inverse")
    expect_message(cacheSolve(cm), "getting cached data")
    m2 <- matrix(c(4,5,6,7),2,2)
    cm$set(m2)
    expect_message(cacheSolve(cm), "solving and caching new inverse")
    expect_message(cacheSolve(cm), "getting cached data")
})

