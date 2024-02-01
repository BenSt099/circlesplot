library("plotrix")

plot(0, 0, type = "n", xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), axes="n", asp=1)
draw.circle(0, 0, 3, lwd=2, col = "navyblue")
draw.circle(0, 0, 1, lwd=2, col = "dodgerblue2")
text(0, -3.4, "circlesplot", col = "deeppink4", font = 2)
