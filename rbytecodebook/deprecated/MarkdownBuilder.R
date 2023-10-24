
library(R6)

# [`AND` -@sec-AND6 ]
xref <- function(ref, title) {
  sprintf("[%s  Section -@sec-%s]", title, ref)
}

as_code <- function(x) {
  paste0("`", x, "`")
}

as_bold <- function(x) {
  paste0("**", x, "**")
}


MDBuilder <- R6::R6Class(
  "MDBuilder",
  
  public = list(
    doc = NULL,
    
    initialize = function() {
      self$doc = list()
    },
    
    add = function(x) {
      self$doc <- c(self$doc, c(x, ""))
      invisible(self)
    },
    
    hx = function(prefix, title, ref = NULL) {
      if (!is.null(ref)) {
        ref <- paste0("{#sec-", ref, "}")
      }
      x <- paste(prefix, title, ref)
      self$add(x)
      invisible(self)
    },
    
    h1 = function(title, ref = NULL) {
      self$hx("#", title, ref)
      invisible(self)
    },
    
    h2 = function(title, ref = NULL) {
      self$hx("##", title, ref)
      invisible(self)
    },
    
    h3 = function(title, ref = NULL) {
      self$hx("###", title, ref)
      invisible(self)
    },
    
    h4 = function(title, ref = NULL) {
      self$hx("####", title, ref)
      invisible(self)
    },
    
    table = function(df, ...) {
      stopifnot(is.data.frame(df))
      x <- knitr::kable(df, ...)
      self$add(x)
      invisible(self)
    },
    
    itemize = function(items) {
      items <- paste("*", items)
      self$add(items)
    },
    
    enumerate = function(items) {
      items <- paste(paste0(seq_along(items), "."), items)
      self$add(items)
    },
    
    code = function(x, ...) {
      opts <- paste(..., sep = ", ")
      x <- paste(x, collapse = "\n")
      x <- paste0("```{r ", opts, "}\n", x, "\n```")
      self$add(x)
    },
    
    comment = function(x, ...) {
      x <- paste(
        '<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
        x,
        '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->'
      )
      
      self$add(x)
    },
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # utils
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {
      paste(unlist(self$doc), collapse = "\n")
    },
    
    print = function(...) {
      cat(self$as_character(...))
    }
    
  )
)

if (FALSE) {
  
  doc <- MDBuilder$new()
  doc$h1("First doc")
  doc$add("Now is the time\n\nThe time is now")
  doc$h2("Subsection goes here")
  doc$table(head(mtcars))
  doc$h1("A list of all my failings:")
  
  words <- readLines("/usr/share/dict/words", n = 10)
  doc$itemize(words)
  
  doc$h2("Let me count the ways")
  doc$enumerate(c("One", "Two", "Three", "Four", "Get on the Dance Floor"))
  doc$code("x + 1")
  
  doc
  
}