#Chapter 7 q1, 5, 9, 17, 27, 29, 31, 35

ch7q1 <- function() {

mean = 8
var = 5/20

print("Mean")
print(mean)
print("Variance")
print(var)

}

ch7q27 <- function() {

x = c(319, 338, 337, 339, 328, 325, 340, 331, 341, 336, 330, 330, 321, 327, 337, 320, 343, 350, 322, 334, 326, 349, 341, 338, 332, 339, 335, 338, 333, 334)

print("Part A")
stem(x)
print("Yes, based on shape of stem and leaf plot")

print("Part B")
m1 = sum(x)/length(x)
m2 = sum(x^2)/length(x)
mean = m1
var = m2 - m1^2
print(mean)
print(var)

print("Part C")
s2 = sum((x - m1)^2)/(length(x)-1)
print(s2)

} 

ch7q29 <-function() {

x = c(319, 338, 337, 339, 328, 325, 340, 331, 341, 336, 330, 330, 321, 327, 337, 320, 343, 350, 322, 334, 326, 349, 341, 338, 332, 339, 335, 338, 333, 334)

u = sum(x) / length(x)
v = sum((x - u)^2) / length(x)
print(u)
print(v)

} 




