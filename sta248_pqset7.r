#8.6: 47 49

ch8q47 <- function() {

x = c(1.0030, 0.9997, 0.9990, 1.0054, 0.9991, 1.0041, 0.9988, 1.0026, 1.0032, 0.9943, 1.0021, 1.0028, 1.0002, 0.9984, 0.9999)
xbar = 1.00084
s = 0.00282
n = length(x)
u = 1
alpha = 0.05
sigma = 0.0025

print("Part A")
t = (xbar - u) / (s/(sqrt(n)))
cp = qt(1-alpha/2, df=n-1)
print("t")
print(t)
print("CP: +-")
print(cp)
print("Since t=1.154 is inside the +- 2.145 CPs, we can accept H0") 

print("Part B")
chi = (n-1) * (s^2) * (1/(sigma^2))
print(chi)
cp = qchisq(0.95, df=14)
print(cp)
print("test stat = 17.81, cp = 23.68, so at alpha=0.05 we can accept H0")

}

ch8q49 <- function() {

n=20
xbar = 28.69
s = 104.93
u = 0
sigma = 150

print("Part A")
cp = qt(0.95, df=n-1)
t = (xbar - u) / (s * (1/(sqrt(n))))
print("CP: +-")
print(cp)
print("t = ")
print(t)
print("since t lies inside the CP interval, at the alpha = 0.1 level we can accept H0")

print("Part B")
cp2= qchisq(0.1, df=n-1)
chi = (n-1) * (s^2) / (sigma^2)
print("CP: ")
print(cp2)
print("Chi: ")
print(chi)
print("since Chi = 9.300 is less than the CP = 11.651, we reject H0")

}