
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a pairlist from a character vector of arguments.
#' 
#' Arguments are either a bare variable name e.g. "x", or a variable name
#' and value e.g. "y = runif(3)"
#' 
#' The RHS of the variable assignment is evaluated to an R expression/call
#' 
#' @examples
#' \dontrun{
#'   create_pairlist(c("x", "y = 3"))
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pairlist <- function(inst_args) {
  
  args <- ifelse(grepl("=", inst_args), inst_args, paste(inst_args, "="))
  pl_txt <- paste0(
    "as.pairlist(alist(",
    paste(args, collapse = ", "),
    "))"
  )
  
  pl <- eval(parse(text = pl_txt))
  
  pl
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard NULL operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Evaluate an expression with particular handling for SPECIAL functions
#' 
#' R has many SPECIAL functions in the base R package that should be called
#' in bytecode with CALLSPECIAL
#' 
#' But given it's a bit difficult to construct a CALLSPECIAL without just
#' writing actual R code, there are some SPECIALs which can be handled
#' by wrapping the special functions in a non-special wrapper.
#' 
#' @param bc bytecode object to be evaluated
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eval_special <- function(bc) {
  
  stopifnot(inherits(bc, 'bytecode'))
  
  # Convert a bunch of {base} special functions into non-special functions
  # by wrapping them here.
  # This means that 'log' can now be written in bytecode assembly with 
  # ops of "GETFUN log", PUSHCONSTARG and CALL
  # Otherwise you'd have to:  CALLSPECIAL log(3)
  env <- as.environment(list(
    "::"       = function(pkg, name)        base::get(name, envir = asNamespace(pkg)),
    ":::"      = function(pkg, name)        utils::getFromNamespace(name, pkg),
    "&&"       = function(x, y)             base::`&&`(x, y),
    "||"       = function(x, y)             base::`||`(x, y),
    .Internal  = function(call)             base::stop(".Internal() not supported in {rbytecode}"),
    call       = function(name, ...)        base::call(name, ...),
    log        = function(x, base = exp(1)) base::log(x, base),
    missing    = function(x)                base::missing(x),
    rep        = function(x, ...)           base::rep(x, ...),
    round      = function(x, digits = 0)    base::round(x, digits),
    signif     = function(x, digits = 6)    base::signif(x, digits),
    substitute = function(expr, env)        base::stop("substitute() not supported in {rbytecode}"),
    switch     = function(EXPR, ...)        base::switch(EXPR, ...)
  ))
  
  eval(bc, envir = env)
}

