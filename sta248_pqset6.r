

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