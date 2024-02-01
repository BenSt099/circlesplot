library("plotrix")

plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1.25, 1.25), axes="n")
draw.circle(0, 0, 0.5)
text(0, -0.9, "Sun: 10", col = "red")

draw.circle(0.7, 0, 0.1)
