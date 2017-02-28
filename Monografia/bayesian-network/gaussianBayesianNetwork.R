## Gaussian Bayesian network.
data(gaussian.test)
fitted = bn.fit(hc(gaussian.test), gaussian.test)
# the result should be around 0.04.
cpquery(fitted,
        event = ((A >= 0) & (A <= 1)) & ((B >= 0) & (B <= 3)),
        evidence = (C + D < 10))