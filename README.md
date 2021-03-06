<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip) [![](http://www.r-pkg.org/badges/version/reprex)](http://www.r-pkg.org/pkg/reprex)

<!-- [![Build Status](https://travis-ci.org/jennybc/reprex.svg?branch=master)](https://travis-ci.org/jennybc/reprex) -->
### reprex

<a href="https://nypdecider.files.wordpress.com/2014/08/help-me-help-you.gif"> <img src="internal/help-me-help-you-still-500-c256.png" width="275" align="right"> </a>

Prepare reproducible examples for posting to [GitHub issues](https://guides.github.com/features/issues/), [Stack Overflow](http://stackoverflow.com/questions/tagged/r), etc.

-   Given R code on the clipboard, in a file, or as expression,
-   run it via `rmarkdown::render()`,
-   with deliberate choices re: arguments and setup chunk.
-   Get resulting runnable code + output as markdown,
-   formatted for target venue, e.g. `gh` or `so`,
-   on the clipboard and, optionally, in a file.
-   Preview an HTML version in RStudio viewer or default browser.

### Installation

``` r
devtools::install_github("jennybc/reprex")
```

### Quick demo

Let's say you copy this code onto your clipboard:

    (y <- 1:4)
    mean(y)

Then you load the `reprex` package and call the main function `reprex()`, where the default target venue is GitHub:

``` r
library(reprex)
reprex()
```

A nicely rendered HTML preview will display in RStudio's Viewer (if you're in RStudio) or your default browser otherwise.

![html-preview](README-viewer-screenshot.png "HTML preview in RStudio")

The relevant bit of GitHub-flavored Markdown is ready to be pasted from your clipboard:

    ``` r
    (y <- 1:4)
    #> [1] 1 2 3 4
    mean(y)
    #> [1] 2.5
    ```

Here's what that Markdown would look like rendered in a GitHub issue:

``` r
(y <- 1:4)
#> [1] 1 2 3 4
mean(y)
#> [1] 2.5
```

Anyone else can copy, paste, and run this immediately.

But wait, there's more!

-   Set the target venue to Stack Overflow with `reprex(venue = "so")`.
-   By default, figures are uploaded to [imgur.com](http://imgur.com) and resulting URL is dropped into an inline image tag.
-   Alternatives to clipboard input:
    -   `reprex(infile = "my_reprex.R")` gets the code from file
    -   `reprex({(y <- 1:4); mean(y)})` gets code from expression

### Reproducible examples

What is a `reprex`? It's a {repr}oducible {ex}ample. Coined by Romain Francois [on twitter](https://twitter.com/romain_francois/status/530011023743655936).

Where and why are they used?

-   A Stack Overflow question that includes a proper reprex is [much more likely to get answered](http://stackoverflow.com/help/no-one-answers), by the most knowledgeable (and therefore busy!) people.
-   A [GitHub issue](https://guides.github.com/features/issues/) that includes a proper reprex is more likely to achieve your goal: getting a bug fixed or getting a new feature, in a finite amount of time.

What are the main requirements?

-   Use the smallest, simplest, most [built-in data](https://stat.ethz.ch/R-manual/R-patched/library/datasets/html/00Index.html) possible.
    -   Think: `iris` or `mtcars`. Bore me.
    -   If you must make some objects, minimize their size and complexity.
    -   Get just a bit of something with `head()` or by indexing with the result of `sample()`. If anything is random, consider using `set.seed()` to make it repeatable.
    -   `dput()` is a good way to get the code to create an object you have lying around. Copy and paste the *result* of this into your reprex. *(We might try to facilitate this in future, see [\#7](https://github.com/jennybc/reprex/issues/7).)*
    -   Look at official examples and try to write in that style. Consider adapting one.
-   Include commands on a strict "need to run" basis.
    -   Ruthlessly strip out anything unrelated to the specific matter at hand.
    -   Include every single command that is required, e.g. loading specific packages via `library(foo)`.
-   Consider including info on your OS and your versions of R version and add-on packages, if it's conceivable that it matters. Use `reprex(..., si = TRUE)` for this.
-   Pack it in, pack it out, and don't take liberties with other people's computers.
    -   If you change options, store original values at the start, do your thing, then restore them: `opar <- par(pch = 19) <blah blah blah> par(opar)`.
    -   If you create files, delete them when you're done: `write(x, "foo.txt") <blah blah blah> file.remove("foo.txt")`.
    -   Don't delete files or objects that you didn't create in the first place.
    -   Don't mask built-in functions, i.e. don't define a new function named `c`.
    -   Take advantage of R's built-in ability to create temporary files and directories. Read up on [`tempfile()` and `tempdir()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html).
-   Whitespace rationing is not in effect. Use good [coding style](http://adv-r.had.co.nz/Style.html).

But won't that take time and effort?

-   Yes, yes it will!
-   80% of the time you will solve your own problem in the course of writing an excellent reprex. YMMV.
-   The remaining 20% of the time, you will create a reprex that is more likely to elicit the desired behavior in others.

Get more concrete details here:

-   [How to make a great R reproducible example?](http://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example/16532098) thread on StackOverflow
-   [How to write a reproducible example](http://adv-r.had.co.nz/Reproducibility.html) from Hadley Wickham's [Advanced R book](http://adv-r.had.co.nz)

### Package philosophy

The reprex code:

-   Must run and, therefore, should be run **by the person posting**. No faking it.
-   Should be easy for others to digest, so **they don't necessarily have to run it**. You are encouraged to include selected bits of output. :scream:
-   Should be easy for others to copy + paste + run, **iff they so choose**. Don't let inclusion of output break executability.

Accomplished like so:

-   use `rmarkdown::render` or, under the hood, `knitr::spin` to run the code and capture output that would display in R console
-   use chunk option `comment = "#>"` to include the output while retaining executability
