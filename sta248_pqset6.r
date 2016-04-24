

ch8q27 <- function() {

print("Part A")
print("H0: p <= 0.50 vs H1: p > 0.50")

print("Part B")
n = 15
p0 = 0.5
expected = n * p0
print(expected)

print("Part C")
alpha = 1 - pbinom(p=0.5, q = 10, size=15)
print(alpha)

print("Part D")
beta1 = pbinom(p=0.6, q=10, size=15)
beta2 = pbinom(p=0.7, q=10, size=15)
beta3 = pbinom(p=0.8, q=10, size=15)
beta4 = pbinom(p=0.9, q=10, size=15)
print("p=0.6")
print(beta1)
print("p=0.7")
print(beta2)
print("p=0.8")
print(beta3)
print("p=0.9")
print(beta4)

print("Part E")
power1 = 1 - beta1
power2 = 1 - beta2
power3 = 1 - beta3
power4 = 1 - beta4
print("p=0.6")
print(power1)
print("p=0.7")
print(power2)
print("p=0.8")
print(power3)
print("p=0.9")
print(power4)

print("Part F")
print("Yes, due to part c")
print("Possible type 1 error since we are rejecting H0 in favor of H1 (it is possible that H0 is correct)")

print("Part G")
print("No, due to part c")
print("Possible type 2 error since we are accepting H0 (possible that H1 is true and we are incorrect)")

}

ch8q29 <- function() {

p1 = 1 - pbinom(p=0.4, q=13, size=20)
p2 = 1 - pbinom(p=0.3, q=13, size=20)
p3 = 1 - pbinom(p=0.2, q=13, size=20)
p4 = 1 - pbinom(p=0.1, q=13, size=20)
print("p = 0.4")
print(p1)
print("p = 0.3")
print(p2)
print("p = 0.2")
print(p3)
print("p = 0.1")
print(p4)

print("yes, less than 0.0577 as expected")


ch8q31 <- function() {

print("Part A")
print("H0: u <= 0.05 vs H1: u > 0.05")

print("Part B")
print("Type 1 error: reject h0 when h0 is true - value of above 5% is assumed but this is incorrect")
print("Type 2 error: fail to reject h0 when h1 is true - value of 5% or below is assumed but this is incorrect")

print("Part C")
n = 100
xbar = 0.051
sd = 0.008
t = (xbar - 0.05) / (sd * (1/(sqrt(n))))
pval = 1 - pt(t, n-1)
print(pval)
print("Debatable whether 0.107 should be rejected or accepted")

}

ch8q33 <- function() {

print("Part A")
print("H0: p <= 0.15 vs H1: p > 0.15")

print("Part B")
n = 40
k = 9
p = 0.15
m = n*p
s = sqrt(p*(1-p)*n)
z = (k - m) / (s)
z1 = pnorm(z, mean=0, sd=1)
print(1-z1)

}

ch8q37 <- function() {

print("Part A")
print("H0: u <= 0.12 vs H1: u > 0.12")

print("Part B")
b = qt(0.99, df=29)
print(b)

print("Part C")
n = 30
sd = 0.03
xbar = 0.135
u = 0.12
p = (xbar - u) / (sd / (sqrt(n)))
print(p)
pval = 1 - pt(p, df=29)
print(pval)
print("h0 is rejected as pval=0.005, which is less than 0.01 (alpha level)")

print("Part D")
print("assume X is normally distributed")

}

ch8q39 <- function() {

print("Part A")
print("H0: u >= 2.5 vs H1: u < 2.5")

print("Part B")
xbar = 1.8
s = 0.8
n = 16
u = 2.5
t = (xbar-u) / (s/(sqrt(n)))
print(t)
pval = pt(t, df=n-1)
print(pval)
print("Since pval = 0.002, reject H0, since strong evidence for H1. Assuming X is normally distributed")

print("Part C")
print("Noise level of these transistors is below 2.5. Reject H0 when it is true: real mean noise is 2.5db")

}

ch8q41 <- function {

print("Part A")
print("H0: u = 4.8 vs H1: u < 4.8")

print("Part B")
n = 200
xbar = 4.7
s = 0.5
u = 4.8
t = (xbar - u) / (s / (sqrt(n)))
print(t)
pval = pt(t, df=n-1)
print(pval)
print("t = -2.828, pval = 0.003, so yes reject H0. Means that level for new coal is below 4.8")

}