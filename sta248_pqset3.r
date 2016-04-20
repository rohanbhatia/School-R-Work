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

	print("Question 27")
	print("A: Plot data in stem and leaf diagram. Reasonable to assume data is normally distrbuted? Explain")
	x = c(319, 338, 337, 339, 328, 325, 340, 331, 341, 336, 330, 330, 321, 327, 337, 320, 343, 350, 322, 334, 326, 349, 341, 338, 332, 339, 335, 338, 333, 334)
	stem(x)
	print("Yes, as the shape of the stem and leaf diagram suggests this")
	print("")
	print("B: Estimate mean, variance using MoM estimators")
	print("m1, sample mean")
	m1 = sum(x) / length(x)
	print(m1)
	print("m2")
	x2 = x*x
	m2  = sum(x2)/30
	print(m2)
	print("var is m2 - m1^2. variance estimate:")
	var = m2 - m1*m1
	print(var)
	print("C: Find an unbiased estimate for variance ")
	print("use sample variance")
	x3 = (x - m1)^2
	s2 = sum(x3)/(length(x) - 1)
	print(s2)
} 

ch7q29 <-function() {

	x = c(319, 338, 337, 339, 328, 325, 340, 331, 341, 336, 330, 330, 321, 327, 337, 320, 343, 350, 322, 334, 326, 349, 341, 338, 332, 339, 335, 338, 333, 334)

	print("Question 29: Using data from q27, what are the MLE estimates for mean and variance?")
	print("Sample mean, pop variance of data")

	u = sum(x) / length(x)

	v = sum((x - u)^2) / length(x)

	print(u)
	print(v)

} 




