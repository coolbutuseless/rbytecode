


if (FALSE) {
  basevars <- ls('package:base', all.names = TRUE)
  types <- sapply(basevars, function(n) typeof(get(n)))
  specials <- names(types)[types == 'special']
  specials
  
  c(
    "::", ":::", ".Internal", "[", "[[", "[[<-", "[<-", "{", "@", 
    "@<-", "&&", "<-", "<<-", "=", "||", "~", "$", "$<-", "break", 
    "call", "expression", "for", "forceAndCall", "function", "if", 
    "log", "missing", "next", "on.exit", "quote", "rep", "repeat", 
    "return", "round", "signif", "substitute", "switch", "UseMethod", 
    "while"
  )
}


test_that("eval_special() works", {

  code <- r"(
  CALLSPECIAL base::mean
  RETURN
  )"
  bc <- asm(code)
  eval(bc)  
  expect_true(TRUE)
  
  code <- r"(
  GETFUN ::
  PUSHCONSTARG "base"
  PUSHCONSTARG "mean"
  CALL
  RETURN
  )"
  bc <- asm(code)
  expect_error(
    eval(bc)
  )
  eval_special(bc)
  expect_true(TRUE)
  
  code <- r"(
  GETFUN ||
  PUSHCONSTARG TRUE
  PUSHCONSTARG FALSE
  CALL
  RETURN
  )"
  bc <- asm(code)
  expect_error(
    eval(bc)
  )
  expect_true(eval_special(bc))
})



if (FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PUSHCONSTARG check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  CALLSPECIAL rbytecode::asm
  RETURN
  )"
  
  code <- r"(
  GETFUN ::
  PUSHCONSTARG "rbytecode"
  PUSHCONSTARG "asm"
  CALL
  RETURN
  )"
  
  
  code <- r"(
  GETFUN ||
  PUSHCONSTARG TRUE
  PUSHCONSTARG FALSE
  CALL
  RETURN
  )"
  
  bcdf <- parse_code(code)
  bcdf
  bcdf$args
  bc <- compile_bcdf(bcdf)
  eval(bc)
  eval_special(bc)

}
