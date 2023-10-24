
library(stringr)
library(testthat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Setup recording of testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ops_test <- lapply(rbytecode::ops, \(x) {x$test <- 0L; x})

find_opnames <- function(code) {
  insts <- stringr::str_trim(stringr::str_split(stringr::str_trim(code), "\n")[[1]])
  opnames <- stringr::str_split(insts, "\\s", simplify = TRUE)[,1]
  opnames <- opnames[!grepl("^@", opnames)]
  opnames <- opnames[!grepl("^#", opnames)]
  opnames
}

count_ops <- function(code) {
  
  for (nm in find_opnames(code)) {
    ops_test[[nm]]$test <<- ops_test[[nm]]$test + 1L
  }
}

run_test <- function(code, expected, ll = list()) {
  bytecode <- asm(code)  
  res      <- eval(bytecode, envir = as.environment(ll))  
  expect_identical(res, expected, label = code)
  
  count_ops(code)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check that the disassembly of the compiled bytecode
# matches the parsing of the code to df
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compare_bytecode_df <- function(code) {
  # Test parsing vs disassembly
  bc <- asm(code)
  ref  <- dis(bc)
  test <- parse_code(code)
  
  test$line <- NULL
  
  ref$expr  <- NULL
  test$expr <- NULL
  
  expect_identical(ref, test, label = code)
}


test_that("basic compilation works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' LDCONST
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 1
    LDCONST 2
    ADD
    RETURN
  )"
  expected <- 3
  run_test(code, expected)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 1
    LDCONST 2
    GE
    RETURN
  )"
  expected <- FALSE
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDTRUE
    LDFALSE
    UMINUS
    LT
    RETURN
  )"
  expected <- FALSE
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' DUP and DUP2ND
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 1
    LDCONST 2
    RETURN
  )"
  expected <- 2
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  code <- r"(
    LDCONST 1
    LDCONST 2
    POP
    RETURN
  )"
  expected <- 1
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  code <- r"(
    LDCONST 1
    LDCONST 2
    DUP
    POP
    RETURN
  )"
  expected <- 2
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  code <- r"(
    LDCONST 1
    LDCONST 2
    DUP2ND
    RETURN
  )"
  expected <- 1
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- function(x) {x + 1}
  code <- r"(
    GETFUN f
    PUSHCONSTARG 1
    CALL
    RETURN
  )"
  expected <- 2
  run_test(code, expected, list(f = f))
  compare_bytecode_df(code)

  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 1
    MATH1 sin
    RETURN
  )"
  expected <- sin(1)
  
  bytecode <- asm(code)  
  res      <- eval(bytecode)  
  expect_identical(res, expected)
  run_test(code, expected, list(f = f))
  
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST TRUE
    LDFALSE
    OR
    LDTRUE
    NOT
    AND
    RETURN
  )"
  expected <- (TRUE || FALSE) && !TRUE
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 1
    LDCONST 2
    MUL
    LDCONST 3
    DIV
    LDCONST 4
    SUB
    EXP
    SQRT
    LOG
    UMINUS
    RETURN
  )"
  expected <- -log(sqrt(exp((1 * 2)/3 - 4)))
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 100
    LDCONST 10
    LOGBASE
    LDCONST 3
    GT
    LDFALSE
    EQ
    EXP
    UPLUS
    RETURN
  )"
  expected <- +exp((log(100, 10) > 3) == FALSE)
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST c(4, 5, 6)
    SEQALONG
    ISINTEGER
    LDTRUE
    LE
    RETURN
  )"
  expected <- is.integer(seq_along(c(4, 5, 6))) <= TRUE
  run_test(code, expected)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDNULL
    ISNULL
    LDTRUE
    NE
    RETURN
  )"
  expected <- is.null(NULL) != TRUE
  run_test(code, expected)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    # This is a comment
    GETVAR x
    AND1ST @label1
    GETVAR y
    AND2ND
    @label1
    RETURN
)"
  x <- TRUE
  y <- FALSE
  expected <- x && y
  run_test(code, expected, list(x = x, y = y))
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    # This is a comment
    GETVAR x
    OR1ST @label1
    GETVAR y
    OR2ND
    @label1
    RETURN
)"
  x <- TRUE
  y <- FALSE
  expected <- x || y
  run_test(code, expected, list(x = x, y = y))
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    # This is a comment
    LDNULL
    ISNULL
    LDCONST "a" 
    ISCHARACTER
    AND
    LDCONST 1i
    ISCOMPLEX
    AND
    LDCONST 1L
    ISINTEGER
    AND
    LDCONST 1.1
    ISNUMERIC
    AND
    LDCONST 2
    ISOBJECT
    NOT
    AND
    LDCONST 3
    ISSYMBOL 
    NOT
    AND
    LDCONST 3.1
    ISDOUBLE
    AND
    LDTRUE
    ISLOGICAL
    AND
    RETURN
)"
  expected <- is.null(NULL) & is.character("a") & is.complex(1i) &
    is.integer(1L) & is.numeric(1.1) & !is.object(2) & !is.symbol(3) &
    is.double(3.1) & is.logical(TRUE)
  run_test(code, expected)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- list(y = 3, z = 4)
  code <- r"(
# This is a comment
GETVAR x
DOLLAR y
RETURN
)"
  expected <- x$y
  run_test(code, expected, list(x = x, y = y))
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
LDCONST 1
LDCONST 5
COLON
RETURN
)"
  expected <- 1:5
  run_test(code, expected)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
LDCONST 1
LDCONST 2
POP
INVISIBLE
RETURN
)"
  expected <- 1
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 3
  SETVAR a
  LDCONST 1
  GETVAR a
  ADD
  RETURN
  )"
  expected <- 4
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' superassign. i.a.  a <<- 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  a <- 1
  code <- r"(
  LDCONST 2
  SETVAR2 a
  INVISIBLE
  RETURN
  )"
  bytecode <- asm(code)  
  res      <- eval(bytecode, env = new.env())  
  expect_identical(a, 2, label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
LDCONST 5
SEQLEN 
STARTSUBSET_N @label1
LDCONST 2:3
VECSUBSET
@label1
RETURN
)"
  expected <- c(2L, 3L)
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
LDCONST list(11, 12, 13)
STARTSUBSET2_N @label1
LDCONST 2
VECSUBSET2
@label1
RETURN
)"
  expected <- 12
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  # This is a comment
  LDCONST 5
  LDCONST 4
  GE
  BRIFNOT @label1
  LDCONST 66
  GOTO @label2
  @label1
  LDCONST 99
  RETURN
  @label2
  RETURN
)"
  bc <- asm(code)
  expected <- 66
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
LDCONST 2
LDCONST 1
SWAP
POP
RETURN
)"
  expected <- 1
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
# This is a comment
GETFUN f
LDCONST 1
PUSHARG
PUSHFALSEARG
PUSHNULLARG
PUSHTRUEARG
CALL
RETURN
)"
  expected <- list(1, FALSE, NULL, TRUE)
  f <- function(...) {list(...)}
  run_test(code, expected, list(f = f))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 2
  INVISIBLE
  VISIBLE
  RETURN
  )"
  expected <- 2
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' c(3, 2, 1)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETBUILTIN c
  PUSHCONSTARG 3
  PUSHCONSTARG 2
  PUSHCONSTARG 1
  CALLBUILTIN
  RETURN
  )"
  expected <- c(3, 2, 1)
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' a <- c(11, 12, 13); a[1] <- 2.22
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 2.22
    STARTASSIGN a
    STARTSUBASSIGN_N @label1
    LDCONST 1L
    VECSUBASSIGN
    @label1
    ENDASSIGN a
    INVISIBLE
    RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  # dis(bytecode)
  a <- c(11, 12, 13)
  res      <- eval(bytecode)  
  expect_identical(a, c(2.22, 12, 13), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' a <- c(11, 12, 13); a[[1]] <- 3.33
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    LDCONST 3.33
    STARTASSIGN a
    STARTSUBASSIGN2_N @label1
    LDCONST 1L
    VECSUBASSIGN2
    @label1
    ENDASSIGN a
    INVISIBLE
    RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  # dis(bytecode)
  a <- c(11, 12, 13)
  res      <- eval(bytecode)  
  expect_identical(a, c(3.33, 12, 13), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' for (i in 1:5) {a <- a + 1}
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 1:5
  STARTFOR i @label1
  @label2
  GETVAR a
  LDCONST 1
  ADD 
  SETVAR a
  POP
  @label1
  STEPFOR @label2
  ENDFOR
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  # dis(bytecode)
  # disq(for (i in 1:5) {a <- a + 1})
  a <- 1
  res <- eval(bytecode)
  expect_identical(a, 6, label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' BCMISMATCH
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  BCMISMATCH
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)
  # dis(bytecode)
  # res <- eval(bytecode)
  expect_error(eval(bytecode), "byte code version mismatch")
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' BCMISMATCH
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 2
  CHECKFUN
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  # dis(bytecode)
  # res <- eval(bytecode)
  expect_error(eval(bytecode), "attempt to apply non-function")
  count_ops(code)
  compare_bytecode_df(code)

  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' 2^3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 2
  LDCONST 3
  EXPT
  RETURN
  )"
  expected <- 8
  run_test(code, expected)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' median(c(1, NA))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETBUILTIN max
  PUSHCONSTARG NA
  PUSHTRUEARG
  SETTAG na.rm
  CALLBUILTIN
  RETURN
  )"
  expected <- -Inf
  suppressWarnings(
    run_test(code, expected)
  )
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a <- c(2, 3); a[] <- 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 1
  STARTASSIGN a
  STARTSUBASSIGN @label1
  DOMISSING
  DFLTSUBASSIGN
  @label1
  ENDASSIGN a
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(a[] <- 1)
  a <- c(2, 3)
  res <- eval(bytecode)
  expect_identical(a, c(1, 1), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a <- c(1, 2, 3); a[1:2] <- 4
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 4
  STARTASSIGN a
  STARTSUBASSIGN2_N @label1
  LDCONST 1:2
  VECSUBASSIGN
  @label1
  ENDASSIGN a
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(a[1:2] <- 4)
  a <- c(1, 2, 3)
  res <- eval(bytecode)
  expect_identical(a, c(4, 4, 3), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # b <- a[2:3]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR a
  STARTSUBSET_N @label1
  LDCONST 2:3
  VECSUBSET
  @label1 
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(a[2:3])
  a <- c(10, 20, 30)
  res <- eval(bytecode)
  expect_identical(res, c(20, 30), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a[[2]]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR a
  STARTSUBSET2_N @label1
  LDCONST 2
  VECSUBSET2 
  @label1 
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(a[[2]])
  a <- c(10, 20, 30)
  res <- eval(bytecode)
  expect_identical(res, c(20), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a[]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR a
  STARTSUBSET @label1
  DOMISSING
  DFLTSUBSET
  @label1 
  RETURN
  )"
  a <- c(2, 3, 4)
  expected <- c(2, 3, 4)
  run_test(code, expected, ll = list(a = a))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[1,2]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR m
  STARTSUBSET_N @label1
  LDCONST 1
  LDCONST 2
  MATSUBSET
  @label1 
  RETURN
  )"
  m <- matrix(1:4, 2, 2)
  m[1,2]
  expected <- 3L
  run_test(code, expected, ll = list(m = m))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[[1,2]]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR m
  STARTSUBSET2_N @label1
  LDCONST 1
  LDCONST 2
  MATSUBSET2
  @label1 
  RETURN
  )"
  m <- matrix(1:4, 2, 2)
  m[[1, 2]]
  expected <- 3L
  run_test(code, expected, ll = list(m = m))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[1,2] <- 5
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 5L
  STARTASSIGN m
  STARTSUBASSIGN_N @label1
  LDCONST 1
  LDCONST 2
  MATSUBASSIGN
  @label1
  ENDASSIGN m
  INVISIBLE
  RETURN
  )"
  m <- matrix(1:4, 2, 2)
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(m[1,2] <- 5L)
  eval(bytecode)
  expect_identical(m, matrix(c(1L, 2L, 5L, 4L), 2, 2), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[[1,2]] <- 5
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 5L
  STARTASSIGN m
  STARTSUBASSIGN2_N @label1
  LDCONST 1
  LDCONST 2
  MATSUBASSIGN2
  @label1
  ENDASSIGN m
  INVISIBLE
  RETURN
  )"
  m <- matrix(1:4, 2, 2)
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(m[[1,2]] <- 5L)
  eval(bytecode)
  expect_identical(m, matrix(c(1L, 2L, 5L, 4L), 2, 2), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[[]] # NOT RUNNABLE 
  # I can't figure out how to get a STARTSUBSET2 from R that is
  # meaningful. so this is a bit hacky and doesn't actually produce
  # runnable R code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR m
  STARTSUBSET2 @label1
  DOMISSING
  DFLTSUBSET2
  @label1
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[[]] <- 1   # NOT RUN
  # I can't figure out how to get a STARTSUBSET2 from R that is
  # meaningful. so this is a bit hacky and doesn't actually produce
  # runnable R code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 1
  STARTASSIGN m
  STARTSUBASSIGN2 @label1
  DOMISSING
  DFLTSUBASSIGN2
  @label1
  ENDASSIGN m
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # matrix:   m[[]] <- 1   # NOT RUN
  # I can't figure out how to get a STARTSUBSET2 from R that is
  # meaningful. so this is a bit hacky and doesn't actually produce
  # runnable R code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR m
  STARTSUBSET @label1
  DOMISSING
  GETVAR_MISSOK i
  PUSHARG
  DFLTSUBSET
  @label1
  RETURN
  )"
  m <- matrix(1:4, 2, 2)
  i <- 1
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  run_test(code, expected = 1:2, ll = list(m =m, i = i))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOT RUN
  # list(...)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  BASEGUARD @label1
  GETFUN list
  DODOTS
  CALL
  @label1
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOT RUN
  # ..2 + 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  DDVAL ..2
  LDCONST 1
  ADD
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOT RUN
  # m[[..1]]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR m
  STARTSUBSET2_N @label1
  DDVAL_MISSOK ..1
  VECSUBSET2
  @label1
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BASEGUARD checks that the expression pointed to by the 'expridx' argument
  # must be a valid function.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  BASEGUARD @label1
  LDCONST 1
  EXP 
  @label1
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  eval(bytecode)
  run_test(code, exp(1), ll = .GlobalEnv)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ... improper user
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  DOTSERR
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  expect_error(
    eval(bytecode),
    "'...' used in an incorrect context"
  )
  count_ops(code)
  compare_bytecode_df(code)

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a$b <- 3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 3
  STARTASSIGN a
  DOLLARGETS b
  ENDASSIGN a
  INVISIBLE
  RETURN
  )"
  a <- list(a = 1, b = 2)
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(a$b <- 3)
  eval(bytecode)
  expect_identical(a, list(a = 1, b = 3), label = code)
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LNK updates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  # Put something on the stack to be protected
  LDCONST 99
  INCLNKSTK
  DECLNKSTK
  LDCONST 1
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  run_test(code, expected = 1)
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 2
  LDCONST 3
  PRINTVALUE
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  res <- eval(bytecode)
  expect_equal(res, 2)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use a RETURNJMP instead of a RETURN
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  LDCONST 1
  RETURNJMP
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  res <- eval(bytecode)
  expect_equal(res, 1)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOT RUN
  # Trigger use of STARTLOOPCNTXT
  # repeat source("thing.R")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  STARTLOOPCNTXT @label1
  @label2
  GETFUN source
  PUSHCONSTARG "thing.R"
  CALL
  POP
  GOTO @label2
  @label1
  ENDLOOPCNTXT
  LDNULL
  INVISIBLE
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(
    repeat source("thing.R")
  )
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # .Call() with nargs <= 16  is a DOTCALL
  # .Call() with nargs > 16 is a CALLBUILTIN
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETVAR hello
  DOTCALL 0
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # .Call() with nargs <= 16  is a DOTCALL
  # .Call() with nargs > 16 is a CALLBUILTIN
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disq(.Call(hello, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
  code <- r"(
  GETBUILTIN .Call
  GETVAR hello
  PUSHARG
  PUSHCONSTARG 1
  CALLBUILTIN
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # SPECIAL functions
  #
  # basevars <- ls('package:base', all.names = TRUE) 
  # types <- sapply(basevars, function(n) typeof(get(n)))
  # names(types)[types == 'special']
  #  [1] "::"           ":::"          ".Internal"    "["            "[["           "[[<-"        
  #  [7] "[<-"          "{"            "@"            "@<-"          "&&"           "<-"          
  # [13] "<<-"          "="            "||"           "~"            "$"            "$<-"         
  # [19] "break"        "call"         "expression"   "for"          "forceAndCall" "function"    
  # [25] "if"           "log"          "missing"      "next"         "on.exit"      "quote"       
  # [31] "rep"          "repeat"       "return"       "round"        "signif"       "substitute"  
  # [37] "switch"       "UseMethod"    "while"       
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  # rep(1, 3)
  code <- r"(
  CALLSPECIAL rep(1, 3)
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(rep(1, 3))
  run_test(code, expected = rep(1, 3))
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BUILTINS
  # basevars <- ls('package:base', all.names = TRUE)
  # types <- sapply(basevars, function(n) typeof(get(n)))
  # names(types)[types == 'builtin'] |> dput()
  # c("-", ":", "!", "!=", "...elt", "...length", "...names", ".C", 
  #   ".cache_class", ".Call", ".Call.graphics", ".class2", ".External", 
  #   ".External.graphics", ".External2", ".Fortran", ".isMethodsDispatchOn", 
  #   ".Primitive", ".primTrace", ".primUntrace", ".subset", ".subset2", 
  #   "(", "*", "/", "&", "%*%", "%/%", "%%", "^", "+", "<", "<=", 
  #   "==", ">", ">=", "|", "abs", "acos", "acosh", "all", "any", "anyNA", 
  #   "Arg", "as.call", "as.character", "as.complex", "as.double", 
  #   "as.environment", "as.integer", "as.logical", "as.numeric", "as.raw", 
  #   "asin", "asinh", "atan", "atanh", "attr", "attr<-", "attributes", 
  #   "attributes<-", "baseenv", "browser", "c", "ceiling", "class", 
  #   "class<-", "Conj", "cos", "cosh", "cospi", "cummax", "cummin", 
  #   "cumprod", "cumsum", "digamma", "dim", "dim<-", "dimnames", "dimnames<-", 
  #   "emptyenv", "enc2native", "enc2utf8", "environment<-", "exp", 
  #   "expm1", "floor", "gamma", "gc.time", "globalenv", "Im", "interactive", 
  #   "invisible", "is.array", "is.atomic", "is.call", "is.character", 
  #   "is.complex", "is.double", "is.environment", "is.expression", 
  #   "is.finite", "is.function", "is.infinite", "is.integer", "is.language", 
  #   "is.list", "is.logical", "is.matrix", "is.na", "is.name", "is.nan", 
  #   "is.null", "is.numeric", "is.object", "is.pairlist", "is.raw", 
  #   "is.recursive", "is.single", "is.symbol", "isS4", "lazyLoadDBfetch", 
  #   "length", "length<-", "levels<-", "lgamma", "list", "log10", 
  #   "log1p", "log2", "max", "min", "Mod", "names", "names<-", "nargs", 
  #   "nzchar", "oldClass", "oldClass<-", "pos.to.env", "proc.time", 
  #   "prod", "range", "Re", "retracemem", "seq_along", "seq_len", 
  #   "seq.int", "sign", "sin", "sinh", "sinpi", "sqrt", "standardGeneric", 
  #   "storage.mode<-", "sum", "tan", "tanh", "tanpi", "tracemem", 
  #   "trigamma", "trunc", "unCfillPOSIXlt", "unclass", "untracemem", 
  #   "xtfrm")
  #
  # .Internal(disassemble(x))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  GETINTLBUILTIN disassemble
  GETVAR x
  PUSHARG
  CALLBUILTIN
  RETURN
  )"
  bytecode <- NULL
  bytecode <- asm(code)  
  dis(bytecode)
  disq(.Internal(disassemble(x)))
  x <- compiler::compile(1)
  run_test(code, expected = .Internal(disassemble(x)), ll =list(x = x))
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GETGLOBFUN not sure this can be created from standard R code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <<- function(x) {x + 1}
  code <- r"(
    GETGLOBFUN f
    PUSHCONSTARG 1
    CALL
    RETURN
  )"
  expected <- 2
  bytecode <- asm(code)
  eval(bytecode)
  run_test(code, expected, list(f = f))
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GETSYMFUN not sure this can be created from standard R code
  #           not sure on syntax to create this in bytecode
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # f <- function(x) {x + 1}
  # code <- r"(
  #   GETSYMFUN f
  #   PUSHCONSTARG 1
  #   CALL
  #   RETURN
  # )"
  # expected <- 2
  # bytecode <- asm(code)
  # dis(bytecode)
  # eval(bytecode)
  # run_test(code, expected, list(f = f))
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # body(f) <- 3 # SETTERCALL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- function(x) {x + 1}
  code <- r"(
  LDCONST 3
  STARTASSIGN f
  GETFUN body<-
  PUSHNULLARG
  SETTER_CALL 4
  ENDASSIGN f
  INVISIBLE 
  RETURN
  )"
  expected <- 2
  bytecode <- asm(code)
  dis(bytecode)
  disq(body(f) <- 3)
  f
  eval(bytecode)
  expect_identical(f(1), expected = 3)
  count_ops(code)
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # switch(x, 10, 20)
  #
  # SWITCH 
  #     NULL | CharVec with "" (BLANK) as the last option
  #     sequence of label positions.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' code <- r"(
  #' GETVAR x
  #' SWITCH NULL / @label1 @label2 @default
  #' @default
  #' LDNULL
  #' INVISIBLE
  #' RETURN
  #' @label1
  #' LDCONST 10
  #' RETURN
  #' @label2
  #' LDCONST 20
  #' RETURN
  #' )"
  #' bytecode <- asm(code)
  #' dis(bytecode)
  #' disq(switch(x, 10, 10))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MAKEPROMISE
  #
  # alist(a = i)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If argument is a constant, then no need for a promise
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- function(x) x + 1
  i <- 2
  
  # f(a = 1)
  code <- r"(
    GETFUN f
    PUSHCONSTARG 1
    SETTAG x
    CALL
    RETURN
    )"
  
  bc <- asm(code)
  dis(bc)
  run_test(code, expected = 2, list(f=f))
  compare_bytecode_df(code)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Catch misformed MAKEPROM/ENDMAKEPROM
  # f(x = i)
  # Nested MAKEPROM not alloed
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    GETFUN f
    MAKEPROM
      GETVAR i
      RETURN
      MAKEPROM
    ENDMAKEPROM
    SETTAG x
    CALL
    RETURN
    )"
  expect_error(asm(code), "MAKEPROM")
  count_ops(code)      
  # compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Catch misformed MAKEPROM/ENDMAKEPROM
  # f(x = i)
  # No Matching ENDMAKEPROM found
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
    GETFUN f
    MAKEPROM
      GETVAR i
      RETURN
    SETTAG x
    CALL
    RETURN
    )"
  expect_error(asm(code), "matching ENDMAKEPROM")
  count_ops(code)      
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # f(x = i)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- function(x) x + 2
  code <- r"(
    GETFUN f
    MAKEPROM
      GETVAR i
      RETURN
    ENDMAKEPROM
    SETTAG x
    CALL
    RETURN
    )"
  bytecode <- asm(code)
  dis(bytecode)
  disq(f(x = i))
  i <- 3
  f(x = i)
  expect_identical(eval(bytecode), 5)
  count_ops(code)     
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MAKECLOSURE
  #   argc = 1
  #   arg = list(formal_args, body(as_bytecode), environment/srcref)
  #
  # function() { x + 1 }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
  MAKECLOSURE
  GETVAR x
  LDCONST 1
  ADD
  RETURN
  MAKECLOSURE
  ENDMAKECLOSURE
  RETURN
  )"
  expect_error(asm(code), "MAKECLOSURE")
  
  
  code <- r"(
  MAKECLOSURE
  GETVAR x
  LDCONST 1
  ADD
  RETURN
  )"
  expect_error(asm(code), "matching ENDMAKECLOSURE")
  
  # function() {x + 1}
  # No formal args. I.e. 'x' is taken from parentenv when evaluating
  code <- r"(
  MAKECLOSURE
  GETVAR x
  LDCONST 1
  ADD
  RETURN
  ENDMAKECLOSURE
  RETURN
  )"
  bytecode <- asm(code)
  dis(bytecode)
  disq(function() {x + 1})
  x <- 3
  f <- eval(bytecode)  # f <- function() {x + 1}
  expect_identical(f(), 4)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  # function(x) {x + 1}
  # Single formal argument, without default value
  code <- r"(
  MAKECLOSURE x
  GETVAR x
  LDCONST 1
  ADD
  RETURN
  ENDMAKECLOSURE
  RETURN
  )"
  bytecode <- asm(code)
  dis(bytecode)
  disq(function(x) {x + 1})
  x <- 3
  f <- eval(bytecode)  # f <- function(x) {x + 1}
  expect_identical(f(1), 2)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  # function(x, y = 9) {x + y}
  # Two formal argument, one with default. one without default.
  code <- r"(
  MAKECLOSURE x; y = 9 
  GETVAR x
  GETVAR y
  ADD
  RETURN
  ENDMAKECLOSURE
  RETURN
  )"
  bytecode <- asm(code)
  dis(bytecode)
  disq(function(x, y) {x + y})
  f <- eval(bytecode)  # f <- function(x, y=9) {x + y}
  expect_identical(f(1), 10)
  expect_identical(f(1, y = 7), 8)
  count_ops(code)
  compare_bytecode_df(code)

  # CLOSURE with NESTED label and GOTO  
  code <- r"(
  MAKECLOSURE
  GETVAR x
  GOTO @label1
  LDCONST 1
  LDCONST 2
  @label1
  LDCONST 7
  ADD
  RETURN
  ENDMAKECLOSURE
  RETURN
  )"
  bytecode <- asm(code)
  dis(bytecode)
  disq(function() {x + 1})
  x <- 3
  f <- eval(bytecode)  # f <- function() {x + 1}
  expect_identical(f(), 3 + 7)
  count_ops(code)
  compare_bytecode_df(code)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Summarise OPCODE testing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ops_tested   <- Filter(\(x) { isTRUE(x$asm) && x$test >  0}, ops_test)
  ops_untested <- Filter(\(x) { isTRUE(x$asm) && x$test == 0}, ops_test)
  ops_notready <- Filter(\(x) {!isTRUE(x$asm)               }, ops_test)
  
  # Haven't worked out how to test these
  ops_untestable <- c(
    'DECLNK', 'DECLNK_N', 'INCLNK',
    'GETSYMFUN',      # No idea how to correctly generate this in bytecode or R. 
    'STARTASSIGN2', 'SUBASSIGN2_N', 'SUBASSIGN_N', 'SUBSET2_N', 'SUBSET_N', 'ENDASSIGN2',
    'GETTER_CALL',
    'STARTC', 'DFLTC' # Now that c() is a BUILTIN this isn't used??
  )
  
  # ops_untested <- setdiff(ops_untested, ops_untestable)
  
  cat("\n-------  Tested ---------\n")  
  cat(sort(names(ops_tested)), "\n")
  
  cat("\n-------  Not ready ---------\n")  
  cat(setdiff(sort(names(ops_notready)), ops_untestable), "\n")
  
  cat("\n-------  Untested ---------\n")  
  cat(setdiff(sort(names(ops_untested)), ops_untestable), "\n")
  
  cat("\n-------  Untestable ---------\n")  
  cat(sort(ops_untestable), "\n")
  
  
})



if (FALSE) {
  
  # MAKECLOSURE
  disq(function(){x + 1})
  
  # MAKEPROM
  disq(f(a=i))
  
  
  fc <- compiler::cmpfun(function() {f(i)})
  dis(fc)
  compiler::disassemble(fc)
  
  
  code <- r"(
   MAKECLOSURE          x
     GETVAR               x 
     LDCONST              1 
     ADD                  
     RETURN               
 ENDMAKECLOSURE       
 RETURN         
  )"
  bc <- asm(code)
  f <- eval(bc)
  f()  
    
  
  disq(
    f(x = )
  )
  
  
}








