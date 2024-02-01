library("plotrix")

#circlesplot(c(1,2,3,4), c('Earth', 'Sun','s','s'))


circlesplot <- function(df_values, df_text, max=10L, line_width=3) {


  if (class(max) != "integer") {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'max' should be integer")
  }
  if (length(df_values) != length(df_text)) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Both vectors should have same length")
  }
  if (is.numeric(df_values) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: One vector should contain numericals")
  }
  if (is.character(df_text) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: One vector should contain characters")
  }

  df <- data.frame(df_values, df_text)
  df <- df[order(df$df_values, decreasing = TRUE),]

  plot(0, 0, type = "n", xlim = c(-9, 9), ylim = c(-9, 9), axes=TRUE, asp=1)

  draw.circle(0, 0, df$df_values[1], lwd=line_width)
  text(0, -0.9, df$df_text[1])

  old <- df$df_values[1]
  c <- old

  df <- df[-c(1), ]

  for (x in df$df_values) {
    c <- c + old + 2
    draw.circle(c, 0, df$df_values[x], lwd=line_width)
    text(0, -0.9, df$df_text[x])
  }


}

