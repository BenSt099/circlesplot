library("plotrix")
#library("viridis")

colors = c('#D1BBD7', '#AE76A3', '#882E72', '#1965B0', '#5289C7', '#7BAFDE', '#4EB265', '#90C987')
#circlesplot(cp_vals=c(5,5,4,5,5,5,2,1), cp_text=c('8','7','6','5','4','3','2','1'), cp_max=3L, cp_title="Planets", cp_color=viridis(8))
circlesplot(cp_vals=c(5,5,4,5,5,5,2,1), cp_text=c('8','7','6','5','4','3','2','1'), cp_max=3L, cp_title="Planets")


circlesplot <- function(cp_vals=NULL, cp_text=NULL, cp_max=10L, cp_line_width=3L, cp_title="", cp_color=NULL, cp_title_size=1.5) {

  .check_params(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color, cp_title_size)

  if (is.null(cp_color)) {
    cp_color <- rep("#FFFFFF", times=length(cp_vals))
    df <- data.frame(cp_vals, cp_text, cp_color)
  }
  df <- data.frame(cp_vals, cp_text, cp_color)
  df <- df[order(df$cp_vals, decreasing = TRUE),]

  .plot_circlesplot(df, cp_line_width, cp_title, cp_max, cp_title_size)
}

.plot_circlesplot <- function(df, cp_line_width, cp_title, cp_max, cp_title_size) {

  diameter <- df$cp_vals[1]
  count <- 0
  x_pos <- 0
  y_pos <- 0
  y_pos_text <- -(y_pos + diameter + 3)
  color_pos <- 1

  par(cex.main = cp_title_size)
  plot(0, 0, type = "n", xlim = c(-10, 40), ylim = c(-40, 20), axes=TRUE, asp=1, main=cp_title, xlab="", ylab="")

  for (item in df$cp_vals) {

    if(count >= cp_max) {

      x_pos <- 0
      y_pos <- y_pos -(3 * diameter)
      count <- 0
      y_pos_text <- y_pos -(diameter + 3)
    }

    draw.circle(x_pos, y_pos, item, lwd=cp_line_width, col = df$cp_color[color_pos])
    text(x_pos, y_pos_text, item)
    x_pos <- x_pos + diameter * 2 + 1
    count <- count + 1
    color_pos <- color_pos + 1
  }
}

.check_params <- function(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color, cp_title_size) {

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
  if (class(cp_title_size) != "numeric") {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_title_size' should be numeric!")
  }
  if (cp_title_size < 1) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_title_size' should be at least 1!")
  }
}
