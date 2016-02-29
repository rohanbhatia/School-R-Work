#Chapter 7 q1, 5, 9, 17, 27, 29, 31, 35


ch7q27 <- function() {

	print("Question 27")
	print("A: Plot data in stem and leaf diagram. Reasonable to assume data is normally distrbuted? Explain")
	x = c(319, 338, 337, 339, 328, 325, 340, 331, 341, 336, 330, 330, 321, 327, 337, 320, 343, 350, 322, 334, 326, 349, 341, 338, 332, 339, 335, 338, 333, 334)
	stem(x)
	print("Yes, as the shape of the stem and leaf diagram suggests this")
}  


ch7q27()