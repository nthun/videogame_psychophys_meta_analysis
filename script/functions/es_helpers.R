# Effect size calculating functions
# TODO: Calculate d based on change score (and not means, sds)
# https://www.researchgate.net/post/How_can_I_calculate_the_effect-size_for_a_repeated_measures_t-test
# https://cran.r-project.org/web/packages/compute.es/compute.es.pdf

# Calculate t values from p (using two-sided p)
p2t <- function(p, n){
    qt((1-p/2), n-1)
}

# Calculate d based means, sds, ns, and r (dependent)
mean2d <- function(m1, m2, sd1, sd2, n1, n2, r){
    ((m2 - m1)/((sd1 + sd2) / 2)) * (1 / (sqrt(2 * (1 - r))))
}

t2d <- function(t, n){
    2*t/sqrt(n - 1)
}

t2d_rm <- function(t, n, r){
    t * sqrt((2 * (1 - r) / n))
}

# Calculate g value from d
d2g <- function(d, n){
    d*(1 - 3/(4 * n - 9))
}

# Calculate 
gvar <- function(g, n, r){
    ((g^2)/(2*(n-1))) + (2*(1-r)/n)
}


mean2dz <- function(md, sdd){
    (md/sdd)
}

