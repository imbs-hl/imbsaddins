#' Count words in selection
#'
#' @description Count the words in the selected text in the source editor.
#'
#' @return Number of words
#'
#' @family word_count
#'
#' @export
selection_word_count <- function() {
  selection <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
  word_count <- sapply(gregexpr("[[:alpha:]]+", selection), function(x) sum(x > 0))
  message(sprintf("%d words in selection.", word_count))
}

#' Count words in active document
#'
#' @description Count the words in the active source editor document.
#'
#' @return Number of words
#' @export
#'
#' @family word_count
document_word_count <- function() {
  content <- rstudioapi::getSourceEditorContext()$contents
  word_count <- sum(sapply(gregexpr("[[:alpha:]]+", content), function(x) sum(x > 0)))
  message(sprintf("%d words in document.", word_count))
}