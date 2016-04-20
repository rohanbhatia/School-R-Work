#Chapter 6 Qs 7, 9, 17, 23, 25, 27, 33

ch6_q7 <- function() {

n = 70
max = 75.1
min = 16.3


print("Part A")
range= max - min
print(range)

print("Part B")
catlength = (range / 7) + 0.1
print(catlength)

print("Part C")
b1 = min - 0.05
print(b1)

print("Part D")
print(b1 + 0*catlength)
print(b1 + 1*catlength)
print(b1 + 2*catlength)
print(b1 + 3*catlength)
print(b1 + 4*catlength)
print(b1 + 5*catlength)
print(b1 + 6*catlength)
print(b1 + 7*catlength)

}

ch6_q9 <- function() {

x  = c(0.2, 0.5, 0.7, 1.1, 1.2, 1.2, 1.3, 1.4, 1.4, 1.4, 1.5, 1.5, 1.6, 1.6, 1.7, 1.9, 2.0, 2.1, 2.1, 2.2, 2.3, 2.5, 2.6, 2.9, 2.8, 3.0, 3.1, 3.0, 3.7, 3.7, 4.0, 4.1, 4.5, 5.1, 5.8, 1.4)

print("Part A")
stem(x)

print("Part B")
print("Yes, skew right")


}

ch6_q17 <- function() {

one = c(1, 3, 2, 2, 5, 4, 4, 3, 3)
two = c(1, 2, 4, 1, 2, 5, 2, 5, 1, 5, 5, 3)

print("Part A")
xbar1 = mean(one)
xbar2 = mean(two)
med1 = median(one)
med2 = median(two)
print("Set 1: Sample mean, sample median")
print(xbar1)
print(med1)
print("Set 2: Sample mean, sample median")
print(xbar2)
print(med2)

print("Part B")
range1 = 5-1
range2 = 5-1
print("Set 1: sample range")
print(range1)
print("Set 2: sample range")
print(range2)

print("Part C")
svar1 = sum((one-xbar1)^2)/(length(one) - 1)
ssd1 = sqrt(svar1)
svar2 = sum((two-xbar2)^2)/(length(two) - 1)
ssd2 = sqrt(svar2)
print("Set 1: sVar, sSD")
print(svar1)
print(ssd1)
print("Set 2: sVar, sSD")
print(svar2)
print(ssd2)

print("Part D")
print("Yes - group 2 has much higher variance")

}

ch6_q23 <- function() {

	print("Chapter 6 Question 23")
	print("Consider the data of Exercise 9")
	x  = c(0.2, 0.5, 0.7, 1.1, 1.2, 1.2, 1.3, 1.4, 1.4, 1.4, 1.5, 1.5, 1.6, 1.6, 1.7, 1.9, 2.0, 2.1, 2.1, 2.2, 2.3, 2.5, 2.6, 2.9, 2.8, 3.0, 3.1, 3.0, 3.7, 3.7, 4.0, 4.1, 4.5, 5.1, 5.8, 1.4)

	print("(a) Find the mean and median for that data")
	mean = mean(x)
	median = median(x)
	print(paste0("Mean: ", mean, " Median: ", median))

	print("(b) Find the standard deviation and variance for these data")
	variance = var(x)
	sd = sqrt(variance)
	print(paste0("Standard deviation: ", sd, " Variance: ", variance))

	print("(c) What physical measurement unit is associated with each of the statistics in parts (a) and (b)?")
	print("Mean, median, sd are in minutes, Variance is unitless")
	print("")
}

ch6_q25 <- function() {

	print("Chapter 6 Question 25")
	x = c(6.2, 5.8, 4.6, 4.9, 7.1, 5.2, 8.1, 0.2, 3.4, 4.5, 8.0, 7.9, 6.1, 5.6, 5.5, 3.1, 6.8, 4.6, 3.8, 2.6, 4.5, 4.6, 7.7, 3.8, 4.1, 6.1, 4.1, 4.4, 5.2, 1.5)

	print("(a) Construct a stem and leaf diagram for these data. Is the assumption justified that X is normally distributed?")
	stem(x)
	print("Yes")

	print("(b) Approximate pop sd via the sample sd s")
	s = sqrt(var(x))
	print(paste0("Sample sd: ", s))

	print("(c) Find the sample range for these data, and use it to approximate pop sd. Compare your result to that obtained in (b)")
	range = max(x) - min(x)
	estimate = range/4
	print(paste0("Estimated sd using range: ", estimate))
	print("This is not far off the sample sd obtained in (b)")

	print("")	
}

ch6_q27 <- function() {

	print("Chapter 6 Question 27")
	print("Let X be normally distributed with mean 0 and variance 1")

	print("(a) Verify that q3 = 0 + 0.67(1), q1 = 0 - 0.67(1)")
	q3 = qnorm(0.75, mean=0, sd=1)
	q1 = qnorm(0.25, mean=0, sd=1)
	print(paste0("q3: ", q3, "    q1: ", q1))

	print("(b) Find the interquartile range for X")
	iqr = q3 - q1
	print(iqr)

	print("(c) Verify that the inner fences for X are f1 = 0 - 2.68(1) and f3 = 0 + 2.68(1)")
	f1 = q1 - 1.5*iqr
	f3 = q3 + 1.5*iqr
	print(paste0("f1: ", f1, "    f3: ", f3))

	print("(d) Verify that the probability that X will fall beyond the inner fences is approx 0.007")
	a = pnorm(f1, mean=0, sd=1)
	b = pnorm(f3, mean=0, sd=1)
	p = 1 - b + a
	print(p)

	print("")
}

ch6_q33 <- function() {
	print("Chapter 6 Question 33")

	x = c(15, 16, 17, 18, 17, 20, 16, 17, 18, 20, 18, 18, 19, 19, 17, 21, 17, 19, 18, 17, 22)

	print("(a)")
	stem(x, scale=2)
	print("Yes")

	k = fivenum(x)
	print("(b)")
	mean = mean(x)
	median = k[3]
	print(paste0("mean: ", mean, " median: ", median))

	print("(c)")
	var = var(x)
	sd = sqrt(var)
	print(paste0("sd: ", sd, " var: ", var))

	print("(d)")
	q1 = k[2]
	q3 = k[4]
	iqr = q3-q1
	print(paste0("Q1: ", q1, " Q3: ", q3, " IQR: ", iqr))
	print("")
}



#MAIN
#ch6_q33()









