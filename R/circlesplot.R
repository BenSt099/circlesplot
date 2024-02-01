library("plotrix")

#plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1.25, 1.25), axes="n")
#draw.circle(0, 0, 0.5)
#text(0, -0.9, "Sun: 10", col = "red")

#draw.circle(0.7, 0, 0.1)

circlesplot <- function(df_values, df_text, max=10L) {


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





}
