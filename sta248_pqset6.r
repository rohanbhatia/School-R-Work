

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