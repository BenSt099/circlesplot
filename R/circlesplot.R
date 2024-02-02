library("plotrix")

circlesplot(cp_vals=c(4,3,2,1), cp_text=c('4','3','2','1'), cp_max=2L, cp_title="Planets")




circlesplot <- function(cp_vals=NULL, cp_text=NULL, cp_max=10L, cp_line_width=3L, cp_title="", cp_color=NULL) {

  .check_params(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color)

  df <- data.frame(cp_vals, cp_text)
  df <- df[order(df$cp_vals, decreasing = TRUE),]

  .plot_circlesplot(df, cp_line_width, cp_title, cp_max)

}


.plot_circlesplot <- function(df, cp_line_width, cp_title, cp_max) {
  plot(0, 0, type = "n", xlim = c(-9, 9), ylim = c(-14, 14), axes=TRUE, asp=1, main=cp_title)

  x_val <- df$cp_vals[1] + df$cp_vals[1] + 1
  y_val <- 0
  first_d <- df$cp_vals[1]
  y_val_text <- - (df$cp_vals[1] + 1)
  counter <- 0

  draw.circle(0, y_val, df$cp_vals[1], lwd=cp_line_width)
  text(0, y_val_text, df$cp_text[1])

  df <- df[-c(1), ]

  for (x in df$cp_vals) {

    counter <- counter + 1

    if(counter == cp_max) {
      x_val <- 0
      y_val <- - (first_d * 2 + 1)
      y_val_text <- - (first_d * 3)
      counter <- 0
    }

    draw.circle(x_val, y_val, x, lwd=cp_line_width)
    text(x_val, y_val_text, x)
    x_val <- x_val + first_d + first_d + 1
  }
}

.check_params <- function(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color) {

  if (class(cp_max) != "integer") {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_max' should be integer!")
  }
  if (cp_max <= 0L) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_max' should be greater than zero!")
  }
  if (class(cp_line_width) != "integer") {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_line_width' should be integer!")
  }
  if (cp_line_width <= 0L) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_line_width' should be greater than zero!")
  }
  if (is.null(cp_vals) == TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' has to be provided!")
  }
  if (is.null(cp_text) == TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_text' has to be provided!")
  }
  if (length(cp_vals) != length(cp_text)) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' and 'cp_text' should have same length!")
  }
  if (is.numeric(cp_vals) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' should contain numericals!")
  }
  if (is.character(cp_text) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_text' should contain characters!")
  }
  if (is.null(cp_color) != TRUE) {
    if (length(cp_color) != length(cp_vals)) {
      stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_color' should have same length as 'cp_vals'!")
    }
  }
}
