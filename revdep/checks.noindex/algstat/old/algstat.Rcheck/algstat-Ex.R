pkgname <- "algstat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('algstat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Amaker")
### * Amaker

flush(stderr()); flush(stdout())

### Name: Amaker
### Title: Distance transitive matrix
### Aliases: Amaker

### ** Examples

Amaker(4, 2)



cleanEx()
nameEx("Emaker")
### * Emaker

flush(stderr()); flush(stdout())

### Name: Emaker
### Title: Create the expected higher-order statistics calculating matrix
###   for approval data
### Aliases: Emaker

### ** Examples

Emaker(6, 0, 1)
Emaker(6, 0, 2)
Emaker(6, 0, 3)
Emaker(6, 0, 4)

Emaker(6, 1, 1)
Emaker(6, 1, 2)
Emaker(6, 1, 3)
Emaker(6, 1, 4)
Emaker(6, 1, 5)
Emaker(6, 1, 6)

# compare to Tmaker
Emaker(6, 1, 3) # contributors when bumping up from 1-groups to 3-groups
Tmaker(6, 3, 1)



cleanEx()
nameEx("Mmaker")
### * Mmaker

flush(stderr()); flush(stdout())

### Name: Mmaker
### Title: Marginals matrix
### Aliases: Mmaker

### ** Examples

data(city)

Mmaker(3)
Mmaker(3) %*% city



cleanEx()
nameEx("Pmaker")
### * Pmaker

flush(stderr()); flush(stdout())

### Name: Pmaker
### Title: Pairs matrix
### Aliases: Pmaker

### ** Examples

data(city)

Pmaker(3)
Pmaker(3) %*% city
# 1 = city, 2 = suburb, 3 = country

# looking just among city folk, generate the pairs matrix
city[,"city",drop=FALSE] # the data
m <- sum(city[,"city"])
k <- (Pmaker(3) %*% city)[,1]
Khat <- upper(k) + lower(m-k)
colnames(Khat) <- row.names(Khat) <- colnames(city)
Khat
round(Khat / m, 2) # % times row is rated over column


# worked out: city is voted over suburb in 123 , 132, and 231, equaling
210 + 23 + 8   # = Khat[1,2]
# whereas suburb is rated over city in 213, 312, 321, equaling
111 + 204 + 81 # = Khat[2,1]


# is there a condorcet choice?

p <- ncol(Khat)
Khat[which(diag(p) == 1)] <- NA
K2 <- t(apply(Khat, 1, function(v) v[!is.na(v)])) # remove diag elts
boole <- apply(K2/m, 1, function(x) all(x > .5))
if(any(boole)) names(boole)[which(boole)]
# suburb is a condorcet choice



cleanEx()
nameEx("Smaker")
### * Smaker

flush(stderr()); flush(stdout())

### Name: Smaker
### Title: Means matrix (rank data)
### Aliases: Smaker

### ** Examples

data(city)

X <- permutations(3)

# the average rank can be computed without this function
normalize <- function(x) x / sum(x)
factorial(3) * apply(t(X) %*% city, 2, normalize)
# the dataset city is really like three datasets; they can be pooled back
# into one via:
rowSums(city)
factorial(3) * apply(t(X) %*% rowSums(city), 2, normalize)


# the means matrix is used to summarize the data to the means subspace
# which is the subspace of m! spanned by the columns of permutations(m)
# note that when we project onto that subspace, the projection has the
# same average rank vector :
Smaker(3) %*% city # the projections, table 2.8
factorial(3) * apply(t(X) %*% Smaker(3) %*% city, 2, normalize)

# the residuals can be computed by projecting onto the orthogonal complement
(diag(6) - Smaker(3)) %*% city # residuals


apply(t(X) %*% city, 2, function(x) x / sum(x) * factorial(3)) # average ranks by group

apply(t(X) %*% rowSums(city), 2, function(x) x / sum(x) * factorial(3)) # average ranks pooled



cleanEx()
nameEx("Tmaker")
### * Tmaker

flush(stderr()); flush(stdout())

### Name: Tmaker
### Title: Create the sufficient statistics calculating matrix for approval
###   data
### Aliases: Tmaker

### ** Examples

Tmaker(4, 2, 0) # m
Tmaker(4, 2, 1) # generates how many of each
Tmaker(4, 2, 2) # gives data (order = subsets(1:4, 2))

Tmaker(5, 2, 0)
Tmaker(5, 2, 1)
Tmaker(5, 2, 2)

Tmaker(4, 3, 0) #
Tmaker(4, 3, 1) # subsets(1:4, 3), 1 is in 1, 2, and 3
Tmaker(4, 3, 2) # subsets(1:4, 2)
Tmaker(4, 3, 3)












data(cookie)


## voting statistics at different levels
############################################################

# projection onto V0: the number of people in survey
effectsOnV0 <- Tmaker(6, 3, 0) %*% cookie$freq
colnames(effectsOnV0) <- "Total Votes"
effectsOnV0 # = sum(cookie$freq)


# projection onto V1: the number of people voting for each cookie
effectsOnV1 <- Tmaker(6, 3, 1) %*% cookie$freq
row.names(effectsOnV1) <- cookie$cookies
colnames(effectsOnV1) <- "Total Votes"
effectsOnV1


# projection onto V2: the number of people voting for each cookie-pair
effectsOnV2 <- Tmaker(6, 3, 2) %*% cookie$freq
row.names(effectsOnV2) <- sapply(subsets(cookie$cookies, 2), paste, collapse = ", ")
colnames(effectsOnV2) <- "Total Votes"
effectsOnV2


# projection onto V3: the number of people voting for each cookie-triple
effectsOnV3 <- Tmaker(6, 3, 3) %*% cookie$freq
row.names(effectsOnV3) <- sapply(subsets(cookie$cookies, 3), paste, collapse = ", ")
colnames(effectsOnV3) <- "Total Votes"
effectsOnV3 # = t(t(cookie$freq)) = the (freq) data



cleanEx()
nameEx("Umaker")
### * Umaker

flush(stderr()); flush(stdout())

### Name: Umaker
### Title: U matrix (rank data)
### Aliases: Umaker

### ** Examples

data(politicalGoals)

lambdas <- apply(partitions(4), 1, function(v) v[v != 0])



cleanEx()
nameEx("bertini")
### * bertini

flush(stderr()); flush(stdout())

### Name: bertini
### Title: Evaluate Bertini Code
### Aliases: bertini

### ** Examples

## Not run: 
##D 
##D # where does the circle intersect the line y = x?
##D code <- "
##D INPUT
##D 
##D variable_group x, y;
##D function f, g;
##D 
##D f = x^2 + y^2 - 1;
##D g = y - x;
##D 
##D END;
##D "
##D bertini(code)
##D 
##D c(sqrt(2)/2, sqrt(2)/2)
##D 
##D 
##D 
##D 
##D # where do the surfaces
##D #  x^2 - y^2 - z^2 - 1/2
##D #  x^2 + y^2 + z^2 - 9
##D #  x^2/4 + y^2/4 - z^2
##D # intersect?
##D #
##D code <- "
##D INPUT
##D 
##D variable_group x, y, z;
##D function f, g, h;
##D 
##D f = x^2 - y^2 - z^2 - 1/2;
##D g = x^2 + y^2 + z^2 - 9;
##D h = x^2/4 + y^2/4 - z^2;
##D 
##D END;
##D "
##D bertini(code)
##D 
##D # algebraic solution :
##D c(sqrt(19)/2, 7/(2*sqrt(5)), 3/sqrt(5)) # +/- each ordinate
##D 
##D 
##D 
##D 
##D # example from bertini manual
##D code <- "
##D INPUT
##D 
##D variable_group x, y;
##D function f, g;
##D 
##D f = x^2 - 1;
##D g = x + y - 1;
##D 
##D END;
##D "
##D out <- bertini(code)
##D summary(out)
##D 
##D 
##D 
##D 
##D 
##D # non zero-dimensional example
##D code <- "
##D CONFIG
##D   TRACKTYPE: 1;
##D END;
##D 
##D INPUT
##D   variable_group x, y, z;
##D   function f1, f2;
##D   f1 = x^2-y;
##D   f2 = x^3-z;
##D END;
##D "
##D out <- bertini(code)
##D # bertini(code, quiet = FALSE) # print broken here
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("bump")
### * bump

flush(stderr()); flush(stdout())

### Name: bump
### Title: Convert Dimensions of Approval Data
### Aliases: bump

### ** Examples

## Not run: 
##D 
##D V0 <- 100 # V0 = number of voters (not votes)
##D bump(V0, 6, 3, 0, 0) # no bump
##D bump(V0, 6, 3, 0, 1) # 1-up
##D bump(V0, 6, 3, 0, 2) # 2-up
##D bump(V0, 6, 3, 0, 3) # 3-up
##D 
##D V1 <- c(30, 40, 50, 50, 60, 70)
##D bump(V1, 6, 3, 1, 0) # bump down
##D bump(V1, 6, 3, 1, 1) # no bump
##D bump(V1, 6, 3, 1, 2) # 1-up
##D bump(V1, 6, 3, 1, 3) # 2-up
##D 
##D cbind(
##D   bump(V1, 6, 3, 1, 2, "popular"),
##D   bump(V1, 6, 3, 1, 2, "even")
##D )
##D 
##D 
##D 
##D 
##D 
##D data(cookie)
##D (out <- spectral(cookie$freq, 6, 3, cookie$cookies))
##D 
##D (V0 <- out$obs$V0)
##D bump(V0, 6, 3, 0, 0)
##D bump(V0, 6, 3, 0, 1)
##D bump(V0, 6, 3, 0, 2)
##D bump(V0, 6, 3, 0, 3)
##D out$fullExp$V0
##D out$decompose(out$effects[,1])
##D 
##D (V1 <- out$obs$V1)
##D bump(V1, 6, 3, 1, 0) # cbind(bump(V1, 6, 3, 1, 0), out$fullExp$V1[[1]])
##D bump(V1, 6, 3, 1, 1) # cbind(bump(V1, 6, 3, 1, 1), out$fullExp$V1[[2]])
##D bump(V1, 6, 3, 1, 2) # cbind(bump(V1, 6, 3, 1, 2), out$fullExp$V1[[3]])
##D bump(V1, 6, 3, 1, 3) # cbind(bump(V1, 6, 3, 1, 3), out$fullExp$V1[[4]])
##D out$fullExp$V1 # the sampler doesn't distribute it's samples up evenly
##D 
##D (V2 <- out$obs$V2)
##D bump(V2, 6, 3, 2, 0) # cbind(bump(V2, 6, 3, 2, 0), out$fullExp$V2[[1]])
##D bump(V2, 6, 3, 2, 1) # cbind(bump(V2, 6, 3, 2, 1), out$fullExp$V2[[2]])
##D bump(V2, 6, 3, 2, 2) # cbind(bump(V2, 6, 3, 2, 2), out$fullExp$V2[[3]])
##D bump(V2, 6, 3, 2, 3) # cbind(bump(V2, 6, 3, 2, 3), out$fullExp$V2[[4]])
##D 
##D (V3 <- out$obs$V3)
##D bump(V3, 6, 3, 3, 0)
##D bump(V3, 6, 3, 3, 1)
##D bump(V3, 6, 3, 3, 2)
##D bump(V3, 6, 3, 3, 3)
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("condorcet")
### * condorcet

flush(stderr()); flush(stdout())

### Name: condorcet
### Title: Find a Condorcet Choice.
### Aliases: condorcet

### ** Examples

data(city)

condorcet(city[,"city"], colnames(city))    # among city-dwellers
condorcet(city[,"suburb"], colnames(city))  # among suburb-dwellers
condorcet(city[,"country"], colnames(city)) # among country-dwellers
condorcet(rowSums(city), colnames(city))    # overall winner



cleanEx()
nameEx("count")
### * count

flush(stderr()); flush(stdout())

### Name: count
### Title: Count Integer Points in a Polytope
### Aliases: count

### ** Examples

## Not run: 
##D 
##D 
##D 
##D 
##D 
##D spec <- c("x + y <= 10", "x >= 1", "y >= 1")
##D count(spec)
##D count(spec, opts = "--dilation=10")
##D count(spec, opts = "--homog")
##D 
##D # by default, the output from LattE is in
##D list.files(tempdir())
##D list.files(tempdir(), recursive = TRUE)
##D 
##D # ehrhart polynomials
##D count(spec, opts = "--ehrhart-polynomial")
##D count(spec, opts = "--ehrhart-polynomial", mpoly = FALSE)
##D 
##D # ehrhart series (raw since mpoly can't handle rational functions)
##D count(spec, opts = "--ehrhart-series")
##D 
##D # simplified ehrhart series - not yet implemented
##D #count(spec, opts = "--simplified-ehrhart-polynomial")
##D 
##D # first 3 terms of the ehrhart series
##D count(spec, opts = "--ehrhart-taylor=3")
##D 
##D # multivariate generating function
##D count(spec, opts = "--multivariate-generating-function")
##D 
##D 
##D # the number of tables with the same marginals
##D data(politics)
##D count(c(
##D   "x11 + x12 == 10",
##D   "x21 + x22 == 10",
##D   "x11 + x21 == 9",
##D   "x12 + x22 == 11",
##D   "x11 >= 0", "x21 >= 0", "x12 >= 0", "x22 >= 0"
##D ))
##D countTables(politics)
##D 
##D 
##D # by vertices
##D spec <- list(c(1,1),c(10,1),c(1,10),c(10,10))
##D count(spec)
##D count(spec, opts = "--vrep")
##D 
##D code <- "
##D 5 3
##D 1 -1 0
##D 1 0 -1
##D 1 -1 -1
##D 0 1 0
##D 0 0 1
##D "
##D count(code)
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("countTables")
### * countTables

flush(stderr()); flush(stdout())

### Name: countTables
### Title: Count Similarly Margined Contingency Tables
### Aliases: countTables

### ** Examples

## Not run: 
##D 
##D 
##D data(politics)
##D countTables(politics)
##D 
##D data(handy)
##D countTables(handy)
##D 
##D data(HairEyeColor)
##D eyeHairColor <- margin.table(HairEyeColor, 2:1)
##D countTables(eyeHairColor)
##D 
##D library(gmp)
##D as.bigz(countTables(eyeHairColor))
##D 
##D 
##D # notice that even tables with small cells can have
##D # huge fibers
##D data(drugs)
##D countTables(drugs)
##D 
##D countTables(eyeHairColor, quiet = FALSE)
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("hierarchical")
### * hierarchical

flush(stderr()); flush(stdout())

### Name: hierarchical
### Title: Fitting Hierarchical Log-linear Models with Algebraic Methods
### Aliases: hierarchical

### ** Examples

## Not run: 
##D 
##D 
##D ## handedness introductory example
##D ############################################################
##D 
##D data(handy)
##D 
##D (out <- hierarchical(~ Gender + Handedness, data = handy))
##D 
##D # hierarchical performs the same tasks as loglin and loglm,
##D # but hierarchical gives the exact test p values and more statistics
##D statsFit <- stats::loglin(handy, list(c(1),c(2)), fit = TRUE, param = TRUE)
##D massFit <- MASS::loglm(~ Gender + Handedness, data = handy)
##D # loglm is just a wrapper of loglin
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D # comparisons between hierarchical and loglin
##D ############################################################
##D 
##D # the expected table given the sufficient statistics can be computed
##D # via two methods, iterative proportional fitting, and the mcmc itself:
##D out$exp # ipf
##D hierarchical(~ Gender + Handedness, data = handy, method = "mcmc")$exp
##D statsFit$fit # the equivalent in loglin; this is used by default in hierarchical
##D 
##D 
##D 
##D 
##D # the parameter values of the loglinear model can be accessed
##D out$param
##D statsFit$param
##D 
##D 
##D 
##D 
##D # the p-value for the overall model is available as well
##D # hierarchical gives the exact conditional p-value
##D # (conditional on the sufficient statistics)
##D # the five numbers correspond the probability of tables that are
##D # "more weird" than the observed table, where "more weird" is determined
##D # by having a larger X2 value (or G2, FT, CR, or NM)
##D out$p.value
##D fisher.test(handy)$p.value # out$p.value["X2"] is accurate to monte carlo error
##D 
##D 
##D # loglin gives the p-values using the unconditional asymptotic distributions
##D c(
##D   "X2" = pchisq(statsFit$pearson, df = statsFit$df, lower.tail = FALSE),
##D   "G2" = pchisq(statsFit$lrt, df = statsFit$df, lower.tail = FALSE)
##D )
##D 
##D out$mid.p.value # the mid (exact conditional) p-value is also available
##D 
##D 
##D 
##D 
##D # the test statistics based on the observed table and the expected
##D # table under the model are available
##D out$statistic
##D c(statsFit$pearson, statsFit$lrt) # loglin only gives X2 and G2
##D 
##D 
##D 
##D 
##D # the markov basis used for the proposal distribution of the metropolis-hastings
##D # algorithm are returned. the proposal distribution is uniform on +/-
##D # the moves added to the current table
##D out$moves
##D # they are easier understood as tables
##D vec2tab(out$moves, dim(handy))
##D # notice that the marginals stay fixed:
##D handy + vec2tab(out$moves, dim(handy))
##D 
##D 
##D 
##D 
##D # these were computed as the markov basis of the integer matrix
##D out$A
##D markov(out$A)
##D out$moves
##D 
##D 
##D 
##D 
##D # the moves are also sometimes written in tableau form (LAS p.13)
##D tableau(out$moves, dim(handy))
##D # that's +1 the the table in elements [1,1] and [2,2]
##D # and -1 in the table in elements [1,2] and [2,1]
##D 
##D 
##D 
##D 
##D # the acceptance probability of the MCMC is retained
##D out$acceptProb
##D 
##D 
##D 
##D 
##D # various model assessment measures are also available
##D out$quality
##D 
##D 
##D 
##D 
##D # the number of independent parameters per term are in df
##D out$df
##D 
##D 
##D 
##D 
##D # as an added help, you may find the visuals in vcd useful:
##D # library(vcd)
##D # mosaic(~ Gender + Handedness, data = handy, shade = TRUE, legend = TRUE)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## politics example - with computing the exact p value by hand
##D ############################################################
##D 
##D data(politics)
##D 
##D (out <- hierarchical(~ Personality + Party, data = politics))
##D statsFit <- stats::loglin(politics, as.list(1:2), fit = TRUE, param = TRUE)
##D 
##D out$p.value
##D # exact without monte-carlo error
##D sum(dhyper(c(0:3,6:9), 10, 10, 9))
##D fisher.test(politics)$p.value
##D round(dhyper(0:9, 10, 10, 9), 4)
##D 
##D 
##D # comparisons :
##D out$exp
##D statsFit$fit
##D 
##D out$param
##D statsFit$param
##D 
##D out$p.value # exact
##D c(
##D   "X2" = pchisq(statsFit$pearson, df = statsFit$df, lower.tail = FALSE),
##D   "G2" = pchisq(statsFit$lrt, df = statsFit$df, lower.tail = FALSE)
##D ) # asymptotic approximation
##D fisher.test(politics)$p.value # accurate to monte carlo error
##D 
##D out$statistic # accurate to monte carlo error
##D c(statsFit$pearson, statsFit$lrt)
##D 
##D # mosaic(~ Personality + Party, data = politics, shade = TRUE, legend = TRUE)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## eyeHairColor from the Diaconis and Sturmfels reference
##D ############################################################
##D 
##D data(HairEyeColor)
##D eyeHairColor <- margin.table(HairEyeColor, 2:1)
##D 
##D outC <- hierarchical(~ Eye + Hair, data = eyeHairColor)
##D outR <- hierarchical(~ Eye + Hair, data = eyeHairColor, engine = "R")
##D 
##D # doesn't work even with workspace = 2E9 (with over 4.5Gb in memory)
##D #fisher.test(eyeHairColor, hybrid = TRUE, workspace = 2E9)
##D 
##D tableau(outC$moves, dim(eyeHairColor))
##D 
##D 
##D # library(microbenchmark)
##D # microbenchmark(
##D #   hierarchical(~ Eye + Hair, data = eyeHairColor),
##D #   hierarchical(~ Eye + Hair, data = eyeHairColor, engine = "R")
##D # )
##D # 5-10 times faster; much faster with increased iter
##D 
##D 
##D # mosaic(~ Eye + Hair, data = HairEyeColor, shade = TRUE, legend = TRUE)
##D 
##D 
##D 
##D 
##D 
##D 
##D ## abortion preference example from the
##D ## Diaconis and Sturmfels reference pp. 379--381
##D ## a no 3-way interaction model
##D ############################################################
##D 
##D data(abortion)
##D 
##D out <- hierarchical(
##D   ~ Education*Abortion + Abortion*Denomination + Education*Denomination,
##D   data = abortion,
##D   iter = 10000, burn = 50000, thin = 50
##D )
##D out$p.value
##D 
##D 
##D vec2tab(rowMeans(out$steps), dim(abortion)) # cf. p. 380
##D loglin(abortion, subsets(1:3, 2), fit = TRUE)$fit
##D 
##D 
##D 
##D out$param
##D loglin(abortion, subsets(1:3, 2), param = TRUE)$param
##D 
##D 
##D 
##D qqplot(rchisq(1055, df = 8), out$sampsStats$X2s)
##D curve(1*x, from = 0, to = 30, add = TRUE, col = "red")
##D 
##D ( nMoves <- 2*ncol(out$moves) ) # DS uses 110
##D # the markov basis is larger than it needs to be
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## loglin no three-way interaction model example
##D ############################################################
##D 
##D # the help for fits the no three-way interaction model on HairEyeColor,
##D # finds a .66196 p-value using the asymptotic distribution, and concludes
##D # a good fit:
##D data(HairEyeColor)
##D 
##D fit <- loglin(HairEyeColor, subsets(1:3, 2), fit = TRUE, param = TRUE)
##D mod <- hierarchical(~ Eye*Hair + Hair*Sex + Eye*Sex, data = HairEyeColor)
##D 
##D 
##D 
##D 
##D # p values
##D pchisq(fit$lrt, fit$df, lower.tail = FALSE) # see ?loglin
##D mod$p.value
##D 
##D # test statistics
##D c(fit$pearson, fit$lrt)
##D mod$statistic
##D 
##D # fits (estimated tables)
##D fit$fit
##D mod$exp
##D mod$obs
##D 
##D 
##D # checking the autocorrelation
##D acf(mod$sampsStats$PRs)
##D mod <- hierarchical(~ Eye*Hair + Hair*Sex + Eye*Sex, data = HairEyeColor, thin = 100)
##D acf(mod$sampsStats$PRs) # got it!
##D 
##D 
##D # the slight differences in fit$fit and mod$exp (both done with ipf from loglin)
##D # are due to differences in variable order:
##D loglin(HairEyeColor, subsets(1:3, 2), fit = TRUE)$fit
##D loglin(HairEyeColor, subsets(1:3, 2)[c(1,3,2)], fit = TRUE)$fit
##D 
##D # a few model moves
##D vec2tab(mod$moves[,1], dim(HairEyeColor))
##D vec2tab(mod$moves[,50], dim(HairEyeColor))
##D -vec2tab(mod$moves[,50], dim(HairEyeColor))
##D 
##D # they contribute 0 to the marginals of the table
##D vec2tab(mod$moves[,50], dim(HairEyeColor))
##D mod$A %*% mod$move[,50]
##D vec2tab(mod$A %*% mod$move[,50], dim(HairEyeColor))
##D 
##D HairEyeColor
##D HairEyeColor + vec2tab(mod$moves[,50], dim(HairEyeColor))
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## a table with positive marginals but no MLE for
##D ## the no-three way interaction model
##D ############################################################
##D 
##D 
##D data(haberman)
##D 
##D mod <- hierarchical(~ X1*X2 + X2*X3 + X1*X3, data = haberman)
##D 
##D statsFit <- loglin(haberman, subsets(1:3, 2), param = TRUE, fit = TRUE)
##D statsFit$fit
##D statsFit$param
##D c(statsFit$pearson, statsFit$lrt)
##D 
##D algstatFit <- hierarchical(~ X1*X2 + X2*X3 + X1*X3, data = haberman, method = "mcmc")
##D algstatFit$exp
##D algstatFit$param
##D algstatFit$statistic
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## an example from agresti, p.322
##D ############################################################
##D 
##D data(drugs)
##D ftable(aperm(drugs, c(3, 1, 2))) # = table 8.3
##D 
##D out <- hierarchical(~Alcohol + Cigarette + Marijuana, data = drugs)
##D matrix(round(aperm(out$exp, c(2,1,3)), 1), byrow = FALSE)
##D 
##D loglin(drugs, as.list(1:3), fit = TRUE)$fit
##D loglin(drugs, as.list(1:3), param = TRUE)$param
##D 
##D # # the saturated model issues a warning from markov, but works :
##D # out <- hierarchical(~Alcohol * Cigarette * Marijuana, data = drugs)
##D # matrix(round(aperm(out$exp, c(2,1,3)), 1), byrow = FALSE)
##D 
##D 
##D ftable(aperm(out$exp, c(3,1,2)))
##D 
##D stats <- loglin(drugs, as.list(1:3), fit = TRUE, param = TRUE)
##D 
##D 
##D # considered via glm
##D 
##D df <- as.data.frame(drugs)
##D mod <- glm(Freq ~ Alcohol + Cigarette + Marijuana, data = df, family = poisson)
##D summary(mod)
##D mod$fitted.values
##D 
##D 
##D # the same can be done with glm :
##D 
##D mod <- glm(
##D   Freq ~ Alcohol + Cigarette + Marijuana,
##D   data = as.data.frame(drugs), family = poisson
##D )
##D summary(mod)
##D matrix(round(mod$fitted.values[c(1,3,2,4,5,7,6,8)],1))
##D 
##D 
##D 
##D mod <- glm(
##D   Freq ~ Alcohol * Cigarette + Marijuana,
##D   data = as.data.frame(drugs), family = poisson
##D )
##D summary(mod)
##D matrix(round(mod$fitted.values[c(1,3,2,4,5,7,6,8)],1))
##D 
##D 
##D mod <- glm(
##D   Freq ~ Alcohol * Cigarette * Marijuana,
##D   data = as.data.frame(drugs), family = poisson
##D )
##D summary(mod)
##D matrix(round(mod$fitted.values[c(1,3,2,4,5,7,6,8)],1))
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("hmat")
### * hmat

flush(stderr()); flush(stdout())

### Name: hmat
### Title: Construct a Hierarchical Model Matrix
### Aliases: hmat

### ** Examples

# LAS example 1.2.11, p.16
varlvls <- c(2,2,2,2)
facets <- list(c(1,2), c(1,4), c(2,3))
( A <- hmat(varlvls, facets) )

# 2x2 independence example
# following convention, the first index indicates rows
varlvls <- c(2,2)
facets <- list(1,2)
( A <- hmat(varlvls, facets) )

printForMarkov <- function(A){
  cat(paste(nrow(A), ncol(A)))
  cat("\n")
  cat(apply(unname(A), 1, paste, collapse = " "), sep = "\n")
  cat("\n")
}
printForMarkov(A)



cleanEx()
nameEx("is.bertini")
### * is.bertini

flush(stderr()); flush(stdout())

### Name: is.bertini
### Title: Bertini Object Check
### Aliases: is.bertini

### ** Examples

# see ?bertini



cleanEx()
nameEx("is.linear")
### * is.linear

flush(stderr()); flush(stdout())

### Name: is.linear
### Title: Test whether an mpoly object is linear.
### Aliases: is.linear

### ** Examples

## Not run: 
##D 
##D is.linear(mp("0"))
##D is.linear(mp("x + 1"))
##D is.linear(mp("x + y"))
##D is.linear(mp(c("0", "x + y")))
##D 
##D is.linear(mp("x + x y"))
##D is.linear(mp(c("x + x y", "x")))
##D 
##D 
## End(Not run)



cleanEx()
nameEx("is.m2")
### * is.m2

flush(stderr()); flush(stdout())

### Name: is.m2
### Title: Macaulay2 Object Check
### Aliases: is.m2

### ** Examples

## Not run: 
##D 
##D is.m2(m2("13^1000"))
##D 
## End(Not run)



cleanEx()
nameEx("kprod")
### * kprod

flush(stderr()); flush(stdout())

### Name: kprod
### Title: Iterated Kronecker product
### Aliases: kprod

### ** Examples

kprod(diag(2), t(ones(2)))
kprod(t(ones(2)), diag(2))

kprod(diag(2), t(ones(2)), t(ones(2)))
kprod(t(ones(2)), diag(2), t(ones(2)))
kprod(t(ones(2)), t(ones(2)), diag(2))


rbind(
  kprod(diag(2), t(ones(2))),
  kprod(t(ones(2)), diag(2))
)



cleanEx()
nameEx("latteMax")
### * latteMax

flush(stderr()); flush(stdout())

### Name: latteMax
### Title: Solve a Linear Program (Maximization)
### Aliases: latteMax

### ** Examples

## Not run: 
##D 
##D latteMax("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
##D 
##D 
##D df <- expand.grid(x = 0:10, y = 0:10)
##D df <- subset(df, x + y <= 10)
##D df$val <- apply(df, 1, function(v) -2*v[1] + 3*v[2])
##D df[which.max(df$val),]
##D 
##D library(ggplot2)
##D qplot(x, y, data = df, size = val)
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("latteMin")
### * latteMin

flush(stderr()); flush(stdout())

### Name: latteMin
### Title: Solve a Linear Program (Minimization)
### Aliases: latteMin

### ** Examples

## Not run: 
##D 
##D latteMin("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
##D latteMin("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"),
##D   method = "cones") # ??
##D 
##D 
##D df <- expand.grid(x = 0:10, y = 0:10)
##D df <- subset(df, x + y <= 10)
##D df$val <- apply(df, 1, function(v) -2*v[1] + 3*v[2])
##D df[which.min(df$val),]
##D 
##D library(ggplot2)
##D qplot(x, y, data = df, size = val)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D latteMin("-2 x - 3 y - 4 z", c(
##D   "3 x + 2 y + z <= 10",
##D   "2 x + 5 y + 3 z <= 15",
##D   "x >= 0", "y >= 0", "z >= 0"
##D ), "cones",quiet = FALSE)
##D 
##D df <- expand.grid(x = 0:10, y = 0:10, z = 0:10)
##D df <- subset(df,
##D   (3*x + 2*y + 1*z <= 10) &
##D   (2*x + 5*y + 3*z <= 15)
##D )
##D 
##D df$val <- apply(df, 1, function(v) -2*v[1] + -3*v[2] + -4*v[3])
##D df[which.min(df$val),]
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("lower")
### * lower

flush(stderr()); flush(stdout())

### Name: lower
### Title: Create a lower triangular matrix
### Aliases: lower

### ** Examples

upper(1:3)
lower(1:3)

upper(1:6)
lower(1:6)

upper(rnorm(6))



cleanEx()
nameEx("lpnorm")
### * lpnorm

flush(stderr()); flush(stdout())

### Name: lpnorm
### Title: Lp Norm
### Aliases: lpnorm

### ** Examples

lpnorm(1:10)
lpnorm(matrix(1:25, 5, 5))
lpnorm(split(1:25, rep(1:5, each = 5)))

lpnorm(1:10, 1)
lpnorm(matrix(1:25, 5, 5), 1)
lpnorm(split(1:25, rep(1:5, each = 5)), 1)

lpnorm(rnorm(10), 0)
lpnorm(matrix(rnorm(25), 5, 5), 0)
lpnorm(split(rnorm(25), rep(1:5, each = 5)), 0)

lpnorm(-5:5, Inf)
lpnorm(matrix(-25:-1, 5, 5), Inf)
lpnorm(split(-25:-1, rep(1:5, each = 5)), Inf)



cleanEx()
nameEx("m2")
### * m2

flush(stderr()); flush(stdout())

### Name: m2
### Title: Evaluate Macaulay2 Code
### Aliases: m2

### ** Examples

## Not run: 
##D 
##D options(digits = 20)
##D 13^20
##D m2("13^20") # correct answer
##D m2("toRR(20,(19004963774880800571392-13^20)/13^20)") # relative error
##D options(digits = 7)
##D 
##D code <- "
##D 1+1
##D 2+3
##D 100!
##D R = QQ[x,y,z]
##D (x+y)^10
##D curve = ideal( x^4-y^5, x^3-y^7 )
##D gens gb curve
##D m = matrix {{x^2, x^2-y^2, x*y*z^7 }}
##D image m
##D R = QQ[a..d]
##D I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
##D G = gens gb I
##D G
##D "
##D m2(code)
##D 
##D 
##D code <- "
##D R = QQ[x,y,z,t]
##D I = ideal( t^4 - x, t^3 - y, t^2 - z)
##D gens gb I
##D "
##D m2(code)
##D 
## End(Not run)



cleanEx()
nameEx("markov")
### * markov

flush(stderr()); flush(stdout())

### Name: markov
### Title: Compute a Markov Basis with 4ti2
### Aliases: markov

### ** Examples

## Not run: 
##D 
##D 
##D 
##D 
##D # 2x2 independence example
##D # following convention, the first index indicates rows
##D varlvls <- c(2,2)
##D facets <- list(1,2)
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D markov(A, "vec")
##D markov(A, "tab", varlvls)
##D markov(A, "tab", varlvls, TRUE)
##D 
##D 
##D 
##D 
##D # 3x3 independence example
##D # following convention, the first index indicates rows
##D varlvls <- c(3,3)
##D facets <- list(1,2)
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D markov(A, "vec")
##D markov(A, "tab", varlvls)
##D markov(A, "tab", varlvls, TRUE)
##D 
##D 
##D 
##D 
##D # LAS example 1.2.1, p.12 (2x3 independence)
##D varlvls <- c(2,3)
##D facets <- list(1, 2)
##D ( A <- hmat(varlvls, facets) )
##D markov(A, "tab", varlvls)
##D # Prop 1.2.2 says that there should be
##D 2*choose(2, 2)*choose(3,2) # = 6
##D # moves.
##D markov(A, "tab", varlvls, TRUE)
##D 
##D 
##D 
##D 
##D 
##D # LAS example 1.2.12, p.17  (no 3-way interaction)
##D varlvls <- c(2,2,2)
##D facets <- list(c(1,2), c(1,3), c(2,3))
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D 
##D 
##D 
##D 
##D 
##D 
##D # LAS example 1.2.12, p.16  (no 3-way interaction)
##D varlvls <- c(2,2,2,2)
##D facets <- list(c(1,2), c(1,4), c(2,3))
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D markov(A, "tab", varlvls) # hard to understand
##D tableau(markov(A), varlvls)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D # using the markov bases database, must be connected to internet
##D # A <- markov(dbName = "ind3-3")
##D B <- markov(hmat(c(3,3), list(1,2)))
##D # all(A == B)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D markov(diag(1, 10))
##D 
## End(Not run)



cleanEx()
nameEx("mchoose")
### * mchoose

flush(stderr()); flush(stdout())

### Name: mchoose
### Title: Multinomial Coefficient
### Aliases: mchoose

### ** Examples

mchoose(6, c(2,2,1,1))



cleanEx()
nameEx("metropolis")
### * metropolis

flush(stderr()); flush(stdout())

### Name: metropolis
### Title: Markov Basis Metropolis-Hastings Algorithm
### Aliases: metropolis

### ** Examples

## Not run: 
##D 
##D 
##D 
##D data(handy)
##D 
##D exp   <- loglin(handy, as.list(1:2), fit = TRUE)$fit
##D e <- unname(tab2vec(exp))
##D h <- t(t(unname(tab2vec(handy))))
##D chisq <- algstat:::computeChisqsCpp(h, e)
##D 
##D out <- hierarchical(~ Gender + Handedness, data = handy)
##D chisqs <- algstat:::computeChisqsCpp(out$steps, e)
##D 
##D mean(chisqs >= chisq)
##D fisher.test(handy)$p.value
##D 
##D 
##D 
##D 
##D 
##D A <- hmat(c(2,2), as.list(1:2))
##D moves <- markov(A)
##D outC <- metropolis(tab2vec(handy), moves, 1e4, engine = "Cpp")
##D str(outC)
##D outR <- metropolis(tab2vec(handy), moves, 1e4, engine = "R", thin = 20)
##D str(outR)
##D 
##D # showSteps(out$steps)
##D 
##D 
##D library(microbenchmark)
##D microbenchmark(
##D   metropolis(tab2vec(handy), moves, engine = "Cpp"),
##D   metropolis(tab2vec(handy), moves, engine = "R")
##D )
##D 
##D # cpp ~ 20-25x faster
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D showSteps <- function(steps){
##D   apply(steps, 2, function(x){
##D     x <- format(x)
##D     tab <- vec2tab(x, dim(handy))
##D     message(
##D       paste(
##D         apply(tab, 1, paste, collapse = " "),
##D         collapse = " "
##D       )
##D     )
##D     message("
##D ", appendLF = F)
##D   })
##D   invisible()
##D }
##D # showSteps(out$steps)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("ones")
### * ones

flush(stderr()); flush(stdout())

### Name: ones
### Title: Ones Vector
### Aliases: ones

### ** Examples

ones(5)
str(ones(5))



cleanEx()
nameEx("polyOptim")
### * polyOptim

flush(stderr()); flush(stdout())

### Name: polyOptim
### Title: Polynomial Optimization
### Aliases: polyOptim

### ** Examples

## Not run: 
##D 
##D # unconstrained optimization of polynomial functions is available
##D polyOptim("x^2")
##D polyOptim("-x^2")
##D polyOptim("-(x - 2)^2")
##D polyOptim("-(x^2 + y^2)")
##D polyOptim("-(x^2 + (y - 2)^2)")
##D 
##D polyOptim("(x - 1) (x - 2) (x - 3)") # fix global labeling
##D 
##D 
##D # constrained optimization over the affine varieties is also available
##D # (affine variety = solution set of polynomial equations)
##D 
##D # find the critical points of the plane f(x,y) = x + y
##D # over the unit circle x^2 + y^2 = 1
##D polyOptim("x + y", "x^2 + y^2 = 1")
##D 
##D # you can specify them as a combo of mpoly, mpolyList, and characters
##D o <- mp("x + y")
##D c <- "x^2 + y^2 = 1"
##D polyOptim(o, c)
##D 
##D c <- mp("x^2 + y^2 - 1")
##D polyOptim(o, c)
##D 
##D out <- polyOptim("x + y", c)
##D str(out)
##D 
##D # another example, note the solutions are computed over the complex numbers
##D polyOptim("x^2 y", "x^2 + y^2 = 3")
##D # solutions: (+-sqrt(2), +-1) and (0, +-sqrt(3))
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("polySolve")
### * polySolve

flush(stderr()); flush(stdout())

### Name: polySolve
### Title: Solve a System of Polynomial Equations
### Aliases: polySolve

### ** Examples

## Not run: 
##D 
##D # it can solve linear systems
##D # (here where the line y = x intersects y = 2 - x)
##D polySolve(c("y", "y"), c("x", "2 - x"), c("x", "y"))
##D 
##D # or nonlinear systems
##D polySolve(c("y", "y"), c("x^2", "2 - x^2"), c("x", "y"))
##D 
##D # perhaps an easier specification is equations themselves
##D # with either the " = " or " == " specifications
##D # varOrder is used to order the solutions returned
##D polySolve(c("y = x^2", "y = 2 - x^2"), varOrder = c("x", "y"))
##D polySolve(c("y == x^2", "y == 2 - x^2"), varOrder = c("x", "y"))
##D 
##D 
##D # mpoly objects can be given instead of character strings
##D lhs <- mp(c("y - (2 - x)", "x y"))
##D rhs <- mp(c("0","0"))
##D polySolve(lhs, rhs, varOrder = c("x", "y"))
##D 
##D # if no default right hand side is given, and no "=" or "==" is found,
##D # rhs is taken to be 0's.
##D # below is where the lines y = x and y = -x intersect the unit circle
##D polySolve(c("(y - x) (y + x)", "x^2 + y^2 - 1"))
##D 
##D # the output object is a bertini object
##D out <- polySolve(c("(y - x) (y + x)", "x^2 + y^2 - 1"))
##D str(out,1)
##D 
##D # here is the code that was run :
##D cat(out$bertiniCode)
##D 
##D # the finite and real solutions:
##D out$finite_solutions
##D out$real_finite_solutions
##D 
##D 
##D 
##D 
##D # example from Riccomagno (2008), p. 399
##D polySolve(c(
##D   "x (x - 2) (x - 4) (x - 3)",
##D   "(y - 4) (y - 2) y",
##D   "(y - 2) (x + y - 4)",
##D   "(x - 3) (x + y - 4)"
##D ))
##D 
## End(Not run)



cleanEx()
nameEx("print.bertini")
### * print.bertini

flush(stderr()); flush(stdout())

### Name: print.bertini
### Title: Pretty Printing of Bertini Output
### Aliases: print.bertini

### ** Examples

## Not run: 
##D 
##D # see ?bertini
##D 
##D variety("x^2 + 1")
##D variety(c("x^2 + 1 + y","y"))
##D 
## End(Not run)



cleanEx()
nameEx("print.hierarchical")
### * print.hierarchical

flush(stderr()); flush(stdout())

### Name: print.hierarchical
### Title: Pretty Printing of Hierarchical's Output
### Aliases: print.hierarchical

### ** Examples

# see ?hierarchical



cleanEx()
nameEx("print.m2")
### * print.m2

flush(stderr()); flush(stdout())

### Name: print.m2
### Title: Pretty printing of Macaulay2 output.
### Aliases: print.m2

### ** Examples

## Not run: 
##D 
##D m2("13^1000")
##D 
## End(Not run)



cleanEx()
nameEx("print.polyOptim")
### * print.polyOptim

flush(stderr()); flush(stdout())

### Name: print.polyOptim
### Title: Pretty printing of polyOptim (Bertini) output.
### Aliases: print.polyOptim

### ** Examples

# see ?polyOptim



cleanEx()
nameEx("print.spectral")
### * print.spectral

flush(stderr()); flush(stdout())

### Name: print.spectral
### Title: Pretty Printing of Spectral's Output
### Aliases: print.spectral

### ** Examples

# see ?spectral



cleanEx()
nameEx("print.tableau")
### * print.tableau

flush(stderr()); flush(stdout())

### Name: print.tableau
### Title: Pretty printing of tableau output.
### Aliases: print.tableau

### ** Examples

# see ?tableau



cleanEx()
nameEx("projectOnto")
### * projectOnto

flush(stderr()); flush(stdout())

### Name: projectOnto
### Title: Vector Projection onto col(A)
### Aliases: projectOnto

### ** Examples

A <- diag(5)[,1:2]
x <- 1:5
projectOnto(A, x)



cleanEx()
nameEx("projectOntoPerp")
### * projectOntoPerp

flush(stderr()); flush(stdout())

### Name: projectOntoPerp
### Title: Vector Projection onto the orthogonal complement of col(A)
### Aliases: projectOntoPerp

### ** Examples

A <- diag(5)[,1:2]
x <- 1:5
projectOnto(A, x)



cleanEx()
nameEx("rvotes")
### * rvotes

flush(stderr()); flush(stdout())

### Name: rvotes
### Title: Random Spectral Data
### Aliases: rvotes

### ** Examples

rvotes(100, 10, 3)



cleanEx()
nameEx("setBertiniPath")
### * setBertiniPath

flush(stderr()); flush(stdout())

### Name: setBertiniPath
### Title: Set Bertini Path
### Aliases: setBertiniPath

### ** Examples

## Not run: 
##D 
##D setBertiniPath()
##D 
## End(Not run)



cleanEx()
nameEx("setLattePath")
### * setLattePath

flush(stderr()); flush(stdout())

### Name: setLattePath
### Title: Set LattE Path
### Aliases: setLattePath

### ** Examples

## Not run: 
##D 
##D setLattePath()
##D 
## End(Not run)



cleanEx()
nameEx("setM2Path")
### * setM2Path

flush(stderr()); flush(stdout())

### Name: setM2Path
### Title: Set Macaulay2 Path
### Aliases: setM2Path

### ** Examples

## Not run: 
##D 
##D setM2Path()
##D 
## End(Not run)



cleanEx()
nameEx("setMarkovPath")
### * setMarkovPath

flush(stderr()); flush(stdout())

### Name: setMarkovPath
### Title: Set 4ti2 Path
### Aliases: setMarkovPath

### ** Examples

## Not run: 
##D 
##D setMarkovPath()
##D 
## End(Not run)



cleanEx()
nameEx("spectral")
### * spectral

flush(stderr()); flush(stdout())

### Name: spectral
### Title: Analyze a Rank Dataset
### Aliases: spectral

### ** Examples

## Not run: 
##D 
##D 
##D 
##D ## voting statistics at different levels
##D ############################################################
##D 
##D # load the cookies dataset:
##D data(cookie)
##D cookie$freq
##D cookie$cookies
##D 
##D 
##D # performing the spectral analysis
##D (out <- spectral(cookie$freq, 6, 3, cookie$cookies))
##D 
##D 
##D out$obs # the original observations, and the summary statistics
##D 
##D out$exp # each level is conditional on the previous level's statistics
##D         # (e.g. what you would expect for 1st order effects given sample size)
##D         # these are generated using 10k markov bases based mcmc samples
##D 
##D out$p.value # these are approximate exact test p-values using various
##D             # popular test statistics.  the approximations are good to
##D             # monte carlo error
##D 
##D out$p.value.se # these are the standard errors using the sqrt(p*(1-p)/n)
##D                # asymptotic formula, known to have poor performance
##D                # for small/large p; see package binom for better
##D 
##D out$statistic # the individual statistics are also available
##D               # the values are not comprable across Vi levels (the rows)
##D               # as they have asymptotic chi-squared distributions with
##D               # different degrees of freedom
##D 
##D out$fullExp # you can also get the expected number of samples at each scale
##D             # for tables with the same ith order statistics, i = 0, ..., k-1
##D 
##D 
##D # these can be seen to (re)construct an expected picture of the
##D # complete data given each successive collection of statistics
##D cbind(
##D   obs = cookie$freq,
##D   as.data.frame(lapply(out$fullExp, function(x) round(x[[4]],1)))
##D )[c(2:4,1)]
##D # notice that the reconstruction given only the first order statistics
##D # (the number of individual cookies selected) is quite good
##D 
##D 
##D 
##D # instead of using the reconstructions from the exp coming from
##D # the samples, you could reconstruct the summaries of the observed
##D # data using bump; it's not quite as good :
##D V0 <- bump(cookie$freq, 6, 3, 3, 0)
##D V1 <- bump(cookie$freq, 6, 3, 3, 1)
##D V2 <- bump(cookie$freq, 6, 3, 3, 2)
##D 
##D cbind(
##D   obs = cookie$freq,
##D   round(data.frame(
##D     V0 = bump(V0, 6, 3, 0, 3),
##D     V1 = bump(V1, 6, 3, 1, 3),
##D     V2 = bump(V2, 6, 3, 2, 3)
##D   ), 2)
##D )[c(2:4,1)]
##D 
##D 
##D 
##D 
##D # you can see the model step-by-step with showStages() :
##D out$showStages()
##D # notice (1) the significant reduction in the residuals after conditioning
##D # on the first order statistics and also (2) the powdery noise after
##D # conditioning on the second order statistics.
##D # the p-values reflect the same:
##D #   * the residuals from conditioning on the sample size show the first
##D #     order effects are strongly significant (in out$p.value V1 = 0)
##D #   * the residuals from conditioning on the first order effects suggest
##D #     the second order effects might be significant (V2 ~ .04-.13ish)
##D #   * the residuals from conditioning on the second order effects indicate
##D #     the third order effects are entirely insignificant (V3 > .2)
##D 
##D 
##D # the isotypic subpaces can be used to determine the pure order effects :
##D 
##D out$isotypicBases # bases of the isotypic subspaces (here 4)
##D 
##D out$effects # pure ith order effects; cookie$freq projected onto the bases
##D             # these are their effects at the data level, so they all have
##D             # the same length as the original dataset: choose(n, k)
##D 
##D zapsmall(rowSums(out$effects)) # the effects sum to the data
##D 
##D 
##D # if the Vk effects are 0, then the conclusion is that Vk is perfectly
##D # predicted with the (k-1)st level statistics.  this may lead to the
##D # conclusion that the l2 norms (say) of the effects might be used to
##D # gauge the relative strength of effects :
##D out$effectsNorms # = apply(out$effects, 2, lpnorm)
##D 
##D 
##D # the natural (not full-dimensional) residuals can be seen with the summary
##D out
##D # or with
##D out$residuals
##D # these are the residuals (obs ith level stats) - (exp ith level stats)
##D # given the (i-1)st statistics
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D # bump is a useful function :
##D out$obs
##D bump(cookie$freq, 6, 3, 3, 0) # the 0 level is the number of voters, not votes
##D bump(cookie$freq, 6, 3, 3, 1)
##D bump(cookie$freq, 6, 3, 3, 2)
##D bump(cookie$freq, 6, 3, 3, 3)
##D 
##D V1 <- out$obs$V1 # = bump(cookie$freq, 6, 3, 3, 1)
##D bump(V1, 6, 3, 1, 0)
##D bump(V1, 6, 3, 1, 1)
##D bump(V1, 6, 3, 1, 2) # cbind(bump(V1, 6, 3, 1, 2), out$exp$V2)
##D bump(V1, 6, 3, 1, 3) # cbind(bump(V1, 6, 3, 1, 3), out$fullExp$V1[[4]])
##D # the differences here are between an observation and an expectation
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D out$obs$V1 - out$exp$V1
##D out$residuals$V1
##D out$decompose(out$effects$V1)$V1
##D 
##D out$obs$V2 - out$exp$V2
##D out$residuals$V2
##D 
##D   out$decompose(out$effects$V0)$V2 +
##D   out$decompose(out$effects$V1)$V2 +
##D   out$decompose(out$effects$V2)$V2 -
##D   out$exp$V2
##D 
##D 
##D 
##D 
##D 
##D # this is how to reconstruct the observation given the effects
##D # the cols of out$effects are the Vk order effects reconstructed
##D # from the lower level effects
##D out$obs$V0
##D zapsmall(
##D   out$decompose(out$effects$V0)$V0
##D )
##D 
##D out$obs$V1
##D zapsmall(
##D   out$decompose(out$effects$V0)$V1 +
##D   out$decompose(out$effects$V1)$V1
##D )
##D 
##D out$obs$V2
##D zapsmall(
##D   out$decompose(out$effects$V0)$V2 +
##D   out$decompose(out$effects$V1)$V2 +
##D   out$decompose(out$effects$V2)$V2
##D )
##D 
##D out$obs$V3
##D zapsmall(
##D   out$decompose(out$effects$V0)$V3 +
##D   out$decompose(out$effects$V1)$V3 +
##D   out$decompose(out$effects$V2)$V3 +
##D   out$decompose(out$effects$V3)$V3
##D )
##D zapsmall(rowSums(out$effects))
##D 
##D all(cookie$freq == zapsmall(rowSums(out$effects)))
##D 
##D 
##D 
##D out$effects$V0
##D out$effects$V0 + out$effects$V1
##D out$effects$V0 + out$effects$V2
##D out$effects$V0 + out$effects$V3
##D 
##D 
##D 
##D str(out$sampsDecomposed)
##D as.data.frame(lapply(out$sampsDecomposed, function(l) rowMeans(l$V3)))
##D 
##D eff0 <- rowMeans(out$sampsDecomposed$V0$V3)
##D cbind(eff0, out$effects$V0)
##D 
##D eff1 <- rowMeans(out$sampsDecomposed$V1$V3 - eff0)
##D cbind(eff1, out$effects$V1)
##D 
##D eff2 <- rowMeans(out$sampsDecomposed$V2$V3 - eff0 - eff1)
##D cbind(eff2, out$effects$V2)
##D 
##D sum(eff0)
##D sum(eff1)
##D sum(eff2)
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D str(out$sampsEffectsNorms)
##D 
##D data <- out$sampsEffectsNorms$V0$V3
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D data <- out$sampsEffectsNorms$V0$V2
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D data <- out$sampsEffectsNorms$V0$V1
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D 
##D data <- out$sampsEffectsNorms$V1$V3
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D data <- out$sampsEffectsNorms$V1$V2
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D 
##D data <- out$sampsEffectsNorms$V2$V3
##D plot(density(data))
##D curve(dnorm(x, mean(data), sd(data)), col = "red", add = TRUE)
##D 
##D 
##D 
##D 
##D 
##D 
##D ## how to convert data into the right format
##D ############################################################
##D # this essentially just uses some clever indexing tricks
##D # to reorder the data in the way you want
##D 
##D data <- cookie$raw       # an example raw, unordered dataset
##D levels <- cookie$cookies # the order of the objects you want
##D levsNndcs <- 1:length(levels)
##D names(levsNndcs) <- levels
##D 
##D 
##D # arrange selections within rows (order of selection doesn't matter)
##D data <- t(apply(data, 1, function(x) x[order(levsNndcs[x])] ))
##D 
##D 
##D # arrange rows (order of selectors doesn't matter)
##D for(k in ncol(data):1) data <- data[order(levsNndcs[data[,k]]),]
##D 
##D 
##D # check that you've done the right thing
##D all( data == cookie$sorted )
##D 
##D # the data frequency order should match that of subsets:
##D subsets(levels, 1)
##D 
##D subsets(levels, 2)
##D sapply(subsets(levels, 2), paste, collapse = ", ")
##D 
##D subsets(levels, 3)
##D sapply(subsets(levels, 3), paste, collapse = ", ")
##D 
##D names(cookie$freq)
##D names(cookie$freq) == sapply(subsets(levels, 3), paste, collapse = ", ")
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D 
##D ## other examples
##D ############################################################
##D 
##D # rvotes provides uniform samples
##D 
##D n <- 4
##D k <- 2
##D 
##D raw <- rvotes(250, n, k)
##D rawTogether <- apply(raw, 1, paste, collapse = " ")
##D levels <- sapply(subsets(n, k), paste, collapse = " ")
##D freq <- table( factor(rawTogether, levels = levels) )
##D (out <- spectral(freq, n, k))
##D 
##D out$p.value
##D out$showStages()
##D 
##D out$obs
##D out$exp
##D 
##D 
##D 
##D 
##D 
##D n <- 6
##D k <- 3
##D raw <- rvotes(250, n, k)
##D rawTogether <- apply(raw, 1, paste, collapse = " ")
##D levels <- sapply(subsets(n, k), paste, collapse = " ")
##D freq <- table( factor(rawTogether, levels = levels) )
##D (out <- spectral(freq, n, k))
##D 
##D 
##D n <- 7
##D k <- 3
##D raw <- rvotes(250, n, k)
##D rawTogether <- apply(raw, 1, paste, collapse = " ")
##D levels <- sapply(subsets(n, k), paste, collapse = " ")
##D freq <- table( factor(rawTogether, levels = levels) )
##D (out <- spectral(freq, n, k))
##D 
##D 
##D 
##D n <- 8
##D k <- 3
##D raw <- rvotes(250, n, k)
##D rawTogether <- apply(raw, 1, paste, collapse = " ")
##D levels <- sapply(subsets(n, k), paste, collapse = " ")
##D freq <- table( factor(rawTogether, levels = levels) )
##D # out <- spectral(freq, n, k) # breaks
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("subsets")
### * subsets

flush(stderr()); flush(stdout())

### Name: subsets
### Title: Compute Subsets
### Aliases: subsets

### ** Examples

subsets(1:3)
subsets(1:3, size = 2)
subsets(1:3, include_null = TRUE)

subsets(c('a','b','c','d'))
subsets(c('a','b','c','d'), include_null = TRUE)



cleanEx()
nameEx("summary.bertini")
### * summary.bertini

flush(stderr()); flush(stdout())

### Name: summary.bertini
### Title: Summarize Bertini Output
### Aliases: summary.bertini

### ** Examples

# see ?bertini



cleanEx()
nameEx("tab2vec")
### * tab2vec

flush(stderr()); flush(stdout())

### Name: tab2vec
### Title: Array to Vector conversion
### Aliases: tab2vec

### ** Examples

a <- array(1:24, c(2,3,4))
tab2vec(a)

data(Titanic)
tab2vec(Titanic)
Titanic[1,1,1,1]
Titanic[1,1,1,2]



cleanEx()
nameEx("tableau")
### * tableau

flush(stderr()); flush(stdout())

### Name: tableau
### Title: Tableau Notation for Markov
### Aliases: tableau

### ** Examples

## Not run: 
##D 
##D # 2x2 independence example
##D # following convention, the first index indicates rows
##D varlvls <- c(2,2)
##D facets <- list(1,2)
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D markov(A, "vec")
##D markov(A, "tab", varlvls)
##D markov(A, "tab", varlvls, TRUE)
##D tableau(markov(A), varlvls)
##D 
##D 
##D 
##D 
##D 
##D 
##D # LAS example 1.2.12, p.17  (no 3-way interaction)
##D varlvls <- c(2,2,2)
##D facets <- list(c(1,2), c(1,3), c(2,3))
##D ( A <- hmat(varlvls, facets) )
##D markov(A)
##D 
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("teshape")
### * teshape

flush(stderr()); flush(stdout())

### Name: teshape
### Title: Interconvert data structures
### Aliases: teshape

### ** Examples

data(Titanic)

# array to others
teshape(Titanic, "freq")
teshape(Titanic, "tab") # what it was
teshape(Titanic, "raw")


# freq to others
TitanicFreq <- teshape(Titanic, "freq")
teshape(TitanicFreq, "freq") # what it was
teshape(TitanicFreq, "tab")  # == Titanic
teshape(TitanicFreq, "raw")

# raw to others
TitanicRaw <- teshape(Titanic, "raw")
teshape(TitanicRaw, "freq")
teshape(TitanicRaw, "tab")
teshape(TitanicRaw, "raw")



cleanEx()
nameEx("upper")
### * upper

flush(stderr()); flush(stdout())

### Name: upper
### Title: Create an upper triangular matrix
### Aliases: upper

### ** Examples

upper(1:3)
lower(1:3)

upper(1:6)
lower(1:6)

upper(rnorm(6))



cleanEx()
nameEx("variety")
### * variety

flush(stderr()); flush(stdout())

### Name: variety
### Title: Compute a Variety
### Aliases: variety

### ** Examples

## Not run: 
##D 
##D 
##D polys <- mp(c(
##D   "x^2 - y^2 - z^2 - .5",
##D   "x^2 + y^2 + z^2 - 9",
##D   ".25 x^2 + .25 y^2 - z^2"
##D ))
##D variety(polys)
##D 
##D # algebraic solution :
##D c(sqrt(19)/2, 7/(2*sqrt(5)), 3/sqrt(5)) # +/- each ordinate
##D 
##D 
##D 
##D # character vectors can be taken in; they're passed to mp
##D variety(c("y - x^2", "y - x - 2"))
##D 
##D 
##D 
##D # an example of how varieties are invariant to the
##D # the generators of the ideal
##D variety(c("2 x^2 + 3 y^2 - 11", "x^2 - y^2 - 3"))
##D 
##D # the following takes a few seconds to initialize, feel free to them
##D # gb <- grobner(mp(c("2 x^2 + 3 y^2 - 11", "x^2 - y^2 - 3")))
##D # variety(gb)
##D 
##D m2("
##D R = QQ[x,y]
##D gens gb ideal(2*x^2 + 3*y^2 - 11, x^2 - y^2 - 3)
##D ")
##D variety(c("y^2 - 1", "x^2 - 4"))
##D variety(c("x^2 - 4", "y^2 - 1"))
##D 
##D 
##D 
##D # variable order is by default equal to vars(mpolyList)
##D # (this finds the zeros of y = x^2 - 1)
##D variety(c("y", "y - x^2 + 1")) # y, x
##D vars(mp(c("y", "y - x^2 + 1")))
##D variety(c("y", "y - x^2 + 1"), c("x", "y")) # x, y
##D 
##D 
##D 
##D # complex solutions
##D variety("x^2 + 1")
##D variety(c("x^2 + 1 + y", "y"))
##D 
##D 
##D # multiplicities
##D variety("x^2")
##D variety(c("2 x^2 + 1 + y", "y + 1"))
##D variety(c("x^3 - x^2 y", "y + 2"))
##D 
##D 
##D #
##D p <- mp(c("2 x  -  2  -  3 x^2 l  -  2 x l",
##D   "2 y  -  2  +  2 l y",
##D   "y^2  -  x^3  -  x^2"))
##D variety(p)
##D 
## End(Not run)



cleanEx()
nameEx("vec2tab")
### * vec2tab

flush(stderr()); flush(stdout())

### Name: vec2tab
### Title: Vector to Array conversion
### Aliases: vec2tab

### ** Examples

data(Titanic)
Titanic
tab2vec(Titanic)
vec2tab(tab2vec(Titanic), dim(Titanic))
vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic
all(vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
