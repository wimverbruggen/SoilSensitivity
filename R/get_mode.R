get_mode <- function(x,...){
  mode <- as.numeric(names(sort(table(x), decreasing = TRUE, na.last = TRUE)[1]))
  if (!is.numeric(mode) | length(mode) < 1){
    mode = NA}
  return(mode)
}
