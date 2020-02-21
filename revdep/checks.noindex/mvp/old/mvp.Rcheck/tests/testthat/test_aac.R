## Check aderiv(), which is not easily assessed in the checker()
## framework of aaa.R:

test_that("Test suite aac.R",{

P <- as.mvp("1 + x + x^3 z + 4 x^2 y + 4 z^4")^3
expect_true(aderiv(P,x=1,y=2,z=3) == aderiv(P,z=3,x=1,y=2))

expect_true(lose(as.mvp("0")) + lose(as.mvp("1")) + lose(as.mvp("2")) == 3)

expect_true(aderiv(as.mvp("x^6 y"),x=2,y=1) == as.mvp("30 x^4"),label="aderiv")
})

