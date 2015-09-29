#' Render a reprex
#'
#' Given some R code on the clipboard, in an expression, or in a file, this
#' function runs it via \code{\link[rmarkdown]{render}}. The resulting bit of
#' Markdown is the primary output. It will be ready and waiting on the
#' clipboard, for pasting into a GitHub issue or to stackoverflow. Optionally,
#' the R code and Markdown will be left behind in files. An HTML preview will
#' display in RStudio's Viewer pane, if available, or in the default browser
#' otherwise.
#'
#' @param x An expression. If not given, \code{repex} will look for code in
#'   \code{infile}, if provided, or on the clipboard.
#' @param infile Path to \code{.R} file containing reprex code.
#' @param venue "gh" for GitHub or "so" for stackoverflow.
#' @param outfile Desired stub for output \code{.R}, \code{.md}, and
#'   \code{.html} files for reproducible example. If \code{NULL}, keeps them in
#'   temporary files. At this point, outfiles are deposited in current working
#'   directory, but the goal is to consult options for a place to store all
#'   reprexes.
#' @param show Whether to show rendered output in a viewer (RStudio or browser).
#' @param si Whether to include the results of
#'   \code{\link[devtools]{session_info}}, if available, or
#'   \code{\link{sessionInfo}} at the end of the reprex.
#' @param sandbox Requests that the reprex be run in a clean R session, which
#'   will verify that it is, indeed, a reprex. Defaults to \code{TRUE} whenever
#'   \code{devtools} is installed. When \code{FALSE}, \code{reprex} attempts to
#'   execute in a clean environment, but there are various ways for previous
#'   actions in the user's current session to affect reprex execution, such as
#'   the availability of loaded packages.
#' @param upload.fun Function that is valid for the \code{upload.fun}
#'   \href{http://yihui.name/knitr/options/}{\code{knitr} option}, for uploading
#'   and linking images stored on the web. Defaults to
#'   \code{\link[knitr]{imgur_upload}}.
#'
#' @examples
#' \dontrun{
#' # put some code like this on the clipboard
#' # (y <- 1:4)
#' # mean(y)
#' reprex()
#'
#' # or provide it as code in brackets:
#' reprex({y <- 1:4; mean(y)})
#'
#' # note that you can include newlines in those brackets
#' reprex({
#'   x <- 1:4
#'   y <- 2:5
#'   x + y
#' })
#' }
#'
#' @export
reprex <- function(x, infile = NULL, venue = c("gh", "so"), outfile = NULL,
                   show = TRUE, si = FALSE, sandbox = NULL,
                   upload.fun = knitr::imgur_upload) {

  venue <- match.arg(venue)

  deparsed <- deparse(substitute(x))
  if (identical(deparsed, "")) {
    # no argument was given; use either infile or clipboard
    if (!is.null(infile)) {
      the_source <- readLines(infile)
    } else {
      the_source <- clipr::read_clip()
    }
  } else {
    if (!is.null(infile)) {
      stop("Cannot provide both expression and input file")
    }
    # adjust the deparsed expression
    the_source <- format_deparsed(deparsed)
  }

  if (is.null(sandbox) || sandbox) {
    sandbox <- requireNamespace("devtools", quietly = TRUE)
  } else {
    sandbox <- FALSE
  }

  the_source <- ensure_not_empty(the_source)
  the_source <- ensure_not_dogfood(the_source)
  the_source <- add_header(the_source)
  the_source <- add_si(the_source, si)

  ## TO DO: come back here once it's clear how outfile will be used
  ## i.e., is it going to be like original slug concept?
  r_file <- if (!is.null(outfile)) { outfile } else { tempfile() }
  r_file <- add_ext(r_file)

  writeLines(the_source, r_file)

  r_file <- normalizePath(r_file)

  reprex_(r_file, venue, show, sandbox, upload.fun)
}

reprex_ <- function(r_file, venue = c("gh", "so"), show = TRUE,
                    sandbox = TRUE,
                    upload.fun = knitr::imgur_upload) {

  venue <- match.arg(venue)

  knitr::opts_knit$set(upload.fun = upload.fun)

  rendargs <- list(
    input = r_file,
    output_format = switch(
      venue,
      gh = rmarkdown::md_document(variant = "markdown_github"),
      so = rmarkdown::md_document()
    ),
    envir = switch(
      sandbox,
      `TRUE` = NULL,
      `FALSE` = new.env(parent = as.environment(2))
    ),
    quiet = TRUE)

  if (sandbox) {
    wrapper_file <- gsub("\\.R$", "-wrapper.R", r_file)
    text <- deparse(as.call(c(quote(rmarkdown::render), rendargs)))
    writeLines(text, wrapper_file)
    rendout <-
      suppressMessages(
        try(
          devtools::clean_source(wrapper_file, quiet = TRUE),
          silent = TRUE
        )
      )
    md_file <- gsub("\\.R$", ".md", r_file)
    if (file.exists(md_file))
      rendout <- md_file
    else
      rendout <- FALSE
  } else {
    rendout <-
      suppressMessages(try(do.call(rmarkdown::render, rendargs),
                           silent = TRUE))
  }

  if (inherits(rendout, "try-error") || identical(rendout, FALSE)) {
    stop("\nCannot render this code. Maybe the clipboard contents",
         " are not what you think?\n",
         rendout)
  } else {
    md_outfile <- rendout
  }

  if(venue == "so") {
    md_safe <- readLines(md_outfile)
    writeLines(c("<!-- language: lang-r -->\n", md_safe), md_outfile)
  }

  output_lines <- readLines(md_outfile)
  clipr::write_clip(output_lines)

  html_outfile <- gsub("\\.R", ".html", r_file)
  rmarkdown::render(md_outfile, output_file = html_outfile, quiet = TRUE)

  viewer <- getOption("viewer")

  if (!is.null(viewer) && show) {
    viewer(html_outfile)
  } else if (show) {
    utils::browseURL(html_outfile)
  }

  # return the string output invisibly, useful in tests
  invisible(output_lines)
}
