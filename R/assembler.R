

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse R bytecode assembly into a bytecode data.frame
#' 
#' @param code Single string containing bytecode instructions
#' @return  bytecode data.frame (\code{bcdf})
#' 
#' @examples
#' \dontrun{
#' code <- "LDCONST 1\nLDCONST 2\nADD\nRETURN"
#' parse_code(code)
#' }
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_code <- function(code) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check for a single character string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(code), length(code) == 1, !is.na(code))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the code into individual instructions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  insts_exprs <- stringr::str_trim(stringr::str_split(code, "\n")[[1]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove any trailing comments on a line. 
  # i.e. anything after "##"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  insts_exprs <- sub("##.*?$", "", insts_exprs, perl = TRUE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split off expressions if present
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  insts_exprs <- stringr::str_split(insts_exprs, "\\$\\$", simplify = TRUE)
  
  if (ncol(insts_exprs) == 1) {
    insts <- insts_exprs[,1]
    exprs <- NULL
  } else if (ncol(insts_exprs) == 2) {
    insts <- insts_exprs[,1]
    exprs <- insts_exprs[,2]
  } else {
    stop("parse_code() error related to expressions") 
  }
  insts <- stringr::str_trim(insts)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Expressions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(exprs)) {
    exprs <- vector('list', length(insts))
  } else {
    exprs <- stringr::str_trim(exprs)
    exprs <- lapply(exprs, \(x) {
      if (x == "")
        NULL
      else
        str2lang(x)
    })
    # exprs <- lapply(exprs, \(x) ifelse(x == "", list(NULL), list()))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Init the bytecode data.frame for mustering the meta-information
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df <- data.frame(
    line = seq_along(insts),
    inst = insts
  )
  df$expr <- exprs
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove blanks and comments
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  blank_or_comment <- df$inst == '' | startsWith(df$inst, '#')
  df <- df[!blank_or_comment,]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split instruction into its OP and ARGS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bits      <- stringr::str_split(df$inst, "\\s+")
  
  # OP = first word in row
  df$op     <- vapply(bits, \(x) x[[1]], character(1)) 
  
  # all other words in row
  df$args   <- lapply(bits, \(x) {
    res <- x[-1]
    if (length(res) == 0) {
      NULL
    } else {
      as.list(res)
    }
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check for duplicate labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  labels <- df$op[startsWith(df$op, '@')]
  dupes  <- duplicated(labels)
  if (sum(dupes) > 0) {
    stop("Duplicate labels: ", deparse1(unique(labels[dupes])))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine nested depth for each instruction
  # MAKECLOSURE and MAKEPROM increase the recursion depth
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df$depth <- 0L
  for (i in seq_len(nrow(df) -1L)) {
    if (df$op[i] %in% c('MAKEPROM', 'MAKECLOSURE')) {
      df$depth[i + 1L] <- df$depth[i] + 1L
    } else if (df$op[i] %in% c('ENDMAKEPROM', 'ENDMAKECLOSURE')) {
      df$depth[i] <- df$depth[i - 1L] - 1L
      df$depth[i + 1L] <- df$depth[i]
    } else {
      df$depth[i + 1L] <- df$depth[i]
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tidy arguments
  #
  # For LDCONST and CALLSPECIAL
  # These ops only take 1 argument which could possibly be almost
  # any R value.  The actual R value will have to be parsed from it, 
  # and it could include spaces, so paste() all the arguments back 
  # together.
  #
  # For a MAKECLOSURE. Arguments are separated by ";" not SPACE.
  # Re-split and turn into a pairlist
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_len(nrow(df))) {
    row <- df[i,]
    args <- row$args[[1]]
    args <- switch(
      row$op,
      
      PUSHCONSTARG =,
      SETTER_CALL  =,
      LDCONST      = {
        paste(unlist(args), collapse = " ") |>
          str2lang() |>
          list()
      },
      
      CALLSPECIAL = {
        paste(unlist(args), collapse = " ") |>
          str2lang() |>
          list()
      },
      
      MAKECLOSURE = {
        # Create pairlist representing the formal arguments.
        # If no formals, then formal args pairlist = NULL
        if (length(args) > 0) {
          # formal arguments were specified.
          # Create a text representation e.g. "x; y = 9"
          plargs <- paste(unlist(args), collapse = " ")
          plargs
        } else {
          NULL
        }
      },
      
      DOTCALL = {
        list(as.integer(args))
      },
      
      # default
      args
    )
    
    if (is.null(args)) {
      df$args[i] <- list(NULL)
    } else {
      df$args[[i]] <- args
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check we have the correct number of arguments for each op.
  # Note that in this assembler, we NEVER use the expridx directly
  # so it is always ignored in the argument count
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_len(nrow(df))) {
    row <- df[i,]
    op <- rbytecode::ops[[row$op]]
    
    if (is.null(op)) next
    if (op$name == 'MAKEPROM'   ) next # Custom handling used for this op
    if (op$name == 'MAKECLOSURE') next # Custom handling used for this op
    
    # Check we have the correct number of arguments
    # When writing assembly, the user should NOT specify the 'expridx'
    # argument (this argument is auto-generated by the assembler)
    args <- row$args[[1]]
    nargs <- length(args)
    if (nargs != (op$argc - op$has_expridx)) {
      print(args)
      stop(sprintf(
        "[Line: %i] Wrong num arguments to %s. Expected %i, got %i",
        row$line, row$op, op$argc - op$has_expridx, nargs
      ))
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add Program Counter to data.frame
  # Create a PC (Program Counter)
  #  * Create a Stack of program counters
  #  * When depth increases, push stack
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pc    <- integer(nrow(df))
  stack <- integer(nrow(df))
  ptr   <- 1L
  stack[ptr] <- 1L
  for (i in seq_len(nrow(df))) {
    
    row <- df[i, ]
    op  <- rbytecode::ops[[row$op]]
    
    if (is.null(op)) {
      if (row$op %in% c('ENDMAKECLOSURE', 'ENDMAKEPROM')) {
        ptr <- ptr - 1L
        stopifnot(ptr != 0)
      } 
      pc[i] <- NA_integer_
      next
    }
    
    # Start of this argument
    pc[i] <- stack[ptr]
    
    # Jump ahead by this argument + #args
    stack[ptr] <- stack[ptr] + 1L + op$argc
    
    if (df$op[i] %in% c('MAKECLOSURE', 'MAKEPROM')) {
      ptr <- ptr + 1L
      stack[ptr] <- 1L
    }
  }
  df$pc <- as.integer(pc)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add opcodes to data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  opcode <- integer(nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i,]
    op <- rbytecode::ops[[row$op]]
    if (is.null(op)) {
      opcode[i] <- NA_integer_
    } else {
      opcode[i] <- op$code
    }
  }
  df$opcode <- opcode
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tidy data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df <- df[, c('line', 'depth', 'pc', 'opcode', 'op', 'args', 'expr')]
  
  class(df) <- c('bcdf', class(df))
  rownames(df) <- NULL
  df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Is a particular op actually a label?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_label <- function(x) {
  startsWith(x, "@")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile a bytecode data.frame to an executable R bytecode object
#' 
#' @param bcdf bytecode data.frame
#' @param expr_default The default expression stored with the bytecode.
#'        In general this should be a quoted `call` object, and if you 
#'        are running a standard modern version of R, then it will 
#'        almost never get evaluated/called.
#'        Default: quote(stop("[rbytecode]")).
#' 
#' @examples
#' \dontrun{
#' code <- "LDCONST 1\nLDCONST 2\nADD\nRETURN"
#' bcdf <- parse_code(code)
#' bc   <- compile_bcdf(bcdf)
#' eval(bc)
#' }
#' 
#' 
#' 
#' @importFrom utils tail
#' @import stringr
#' @return bytecode object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compile_bcdf <- function(bcdf, expr_default = quote(stop("[rbytecode]"))) {
  
  stopifnot(is.data.frame(bcdf), nrow(bcdf) > 0)
  
  if (is.null(bcdf$expr)) {
    bcdf$expr <- vector('list', nrow(bcdf))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add a row index when compiling at depth = 0
  # This ".row_idx" will be used in error output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!".row_idx" %in% names(bcdf)) {
    bcdf$.row_idx <- seq_len(nrow(bcdf))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The starting expression given in the compiler is stored as
  # the expression at address = 0.  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- expr_default
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup the context
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  env       <- .GlobalEnv
  options   <- NULL
  srcref    <- NULL
  cenv      <- compiler:::makeCenv(env)
  cntxt     <- compiler:::make.toplevelContext(cenv, options)
  cntxt$env <- compiler:::addCenvVars(cenv, compiler:::findLocals(e, cntxt))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a code buffer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cb <- compiler:::make.codeBuf(e, loc = NULL)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Need to keep track of label names when created.
  # This is solely for the purpose
  # of making sure users don't try and make two labels with the same name!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  labels <- c()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Instructions often want the CONST address of the expression label.
  #
  # This address would be used if the JIT/bytecode interpreter ever failed so that the
  # eval could fallback to the standard R AST interpreter and evaluate the
  # expression stored at this address.
  #
  # But since we're compiling on the fly, there's really no parsed expression
  # to refer to.   (Note: in the future,  could transform the assembly
  # code to create a valid R expression actually representing the code)
  #
  # Most references to the expression index for an instruction are never used 
  # or looked at (exceptions to this rule discussed below) so it doesn't
  # matter what we put here.  If the evaluation of the bytecode fails, it just
  # means that the interpreter is going to evaluate a meaningless expression
  # that we've stored there.
  #
  # It appears that for a CALL.OP the expression index must point
  # to an expression which is a 'call'. 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Walk along the data.frame creating bytecode.
  # Handle MAKEPROM and MAKECLOSURE by recursively calling asm()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- 1L
  while (i <= nrow(bcdf)) {
    row <- bcdf[i, ]
    args <- row$args[[1]]
    
    if (row$op == 'SWITCH') {
      stop("compile_bcdf(): Compiling SWITCH bytecode not yet supported.\n",
           "See Luke Tierney's 'A Byte Code Compiler for R' Sect 12 p67-72\n",
           "for 5 pages of implementation details for SWITCH handling\n")
    } else if (is_label(row$op)) {
      label <- row$op
      if (label %in% labels) {
        stop(sprintf("[Line: %i] Label already defined. Cannot redefine: '%s'", i, label))
      }
      labels <- c(labels, label)
      cb$putlabel(label)
    } else if (row$op == 'MAKEPROM') {
      pos_ENDMAKEPROM <- with(bcdf, which(
        depth == row$depth &
        op    == 'ENDMAKEPROM'
      ))
      idx_ENDMAKEPROM <- pos_ENDMAKEPROM[pos_ENDMAKEPROM > i][1]
      if (length(idx_ENDMAKEPROM) == 0 || is.na(idx_ENDMAKEPROM)) {
        stop("No matching ENDMAKEPROM found for MAKEPROM on LINE: ", row$line)
      }
      prom_df <- bcdf[(i + 1) : (idx_ENDMAKEPROM - 1L),]
      
      # Create bytecode for the promise
      promise <- compile_bcdf(prom_df)
      
      # Setup the argument list for this MAKEPROM 
      bc_args <- list()
      
      # Put the compiled PROMISE into the expression list
      ci <- cb$putconst(promise)
      bc_args[[1L]] <- ci
      
      # add the op + args to the bytecode
      op <- rbytecode::ops[["MAKEPROM"]]
      do.call(cb$putcode, c(op$code, bc_args))
      
      # Advance instruction index to skip over the instructions
      # used in the promise
      i <- idx_ENDMAKEPROM
      
    } else if (row$op == 'MAKECLOSURE') {
      pos_ENDMAKECLOSURE <- with(bcdf, which(
        depth == row$depth &
        op    == 'ENDMAKECLOSURE'
      ))
      idx_ENDMAKECLOSURE <- pos_ENDMAKECLOSURE[pos_ENDMAKECLOSURE > i][1]
      if (length(idx_ENDMAKECLOSURE) == 0 || is.na(idx_ENDMAKECLOSURE)) {
        stop("No matching ENDMAKECLOSURE found for MAKECLOSURE on LINE: ", row$line)
      }
      closure_df <- bcdf[(i + 1) : (idx_ENDMAKECLOSURE - 1L),]
      
      # Create bytecode for the promise
      closure_body <- compile_bcdf(closure_df)
      sref         <- NULL # source refernce
      
      if (length(args) > 0) {
        inst_args <- stringr::str_split(args, ";")[[1]]
        inst_args <- stringr::str_trim(inst_args)
        formal_args <- create_pairlist(inst_args)
      } else {
        formal_args <- NULL
      }
      
      # Closure = formal args + bytecode compiled body + source ref
      closure <- list(formal_args, closure_body, sref)
      
      # Setup the argument list for this MAKECLOSURE
      bc_args <- list()
      
      # Put the compiled PROMISE into the expression list
      ci <- cb$putconst(closure)
      bc_args[[1L]] <- ci
      
      # add the op + args to the bytecode
      op <- rbytecode::ops[["MAKECLOSURE"]]
      do.call(cb$putcode, c(op$code, bc_args))
      
      # Advance instruction index to skip over the instructions
      # used in the promise
      i <- idx_ENDMAKECLOSURE
      
    } else {
      # Standard op
      op <- rbytecode::ops[[row$op]]
      if (is.null(op)) stop("No such instruction: ", row$op)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Loop over the bytecode arguments required by this instruction.
      #
      # Translate any assembly arguments (inst_args) into bytecode arguments (bc_args)
      # handling such things as 
      #     expression index references
      #     consts
      #     names 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      bc_args      <- list()
      inst_arg_idx <- 1L
      
      for (bc_arg_idx in seq_len(op$argc)) {
        arg_type <- op$args[[bc_arg_idx]]
        if (arg_type == 'expridx') {
          # OP references 'expridx' which we don't have for assembly coding
          # MATH1 has special requirements that the call entered at the given
          # expression idx must match the argument to MATH1.OP
          if (!is.null(row$expr[[1]])) {
            # User has specified an expreesion they want here. 
            # Use it - no matter how ridiculous it might be
            ci <- cb$putconst(row$expr[[1]])
            bc_args[[bc_arg_idx]] <- ci
          } else if (row$op == "MATH1") {
            # Have to install an expression with the function name matching
            # the math1 name as R/eval.c does an explicit check and has an error
            # 'math1 compiler/interpreter mismatch' if the expression doesn't
            # match the argument name.
            
            # Get the function name given as an instruction argument
            math1fun <- args[[inst_arg_idx]]
            # Create a quoted call using this function name
            ci <- cb$putconst(bquote(.(as.name(math1fun))(1)))
            bc_args[[bc_arg_idx]] <- ci
            # placing this expression does not consume inst_arg
          } else {
            # it appears that for a CALL.OP the expression index must point
            # to a an expression which is a call!
            # For all other ops, we're going to construct fake expressions
            # that will indicate where the error is.
            # These expressions get printed when an error occurs, but
            # are not actually evaluated.
            err <- bquote(stop(op = .(row$op), row = .(row$.row_idx), line = .(row$line %||% i), depth = .(row$depth)))
            ci <- cb$putconst(err)
            bc_args[[bc_arg_idx]] <- ci
          }
        } else if (arg_type == 'value') {
          # Put this as a raw value in the bytecode. 
          # Used for DOTCALL which takes Number of Args (nargs) as
          # a literal argument
          val <- as.integer(args[[inst_arg_idx]])
          bc_args[[bc_arg_idx]] <- val
          inst_arg_idx <- inst_arg_idx + 1L
        } else if (arg_type == 'asis') {
          # Reference to an expression to be evaluated later 
          # Capture it asis
          # this is used for CALLSPECIAL
          val <- args[[inst_arg_idx]]
          ci <- cb$putconst(val)
          bc_args[[bc_arg_idx]] <- ci
          inst_arg_idx <- inst_arg_idx + 1L
        } else if (arg_type == 'const') {
          # Reference to a constant e.g. 1, 12.3, "a", c(1, 2, 3)
          # first "PUT" the constant into the CONST store, 
          # then add the CONST **idx** to the opcode arguments.
          val <- args[[inst_arg_idx]]
          if (is.language(val)) val <- eval(val)
          ci <- cb$putconst(val)
          bc_args[[bc_arg_idx]] <- ci
          inst_arg_idx <- inst_arg_idx + 1L
        } else if (arg_type == 'name') {
          # Reference to a variable name
          # first "PUT" the constant into the CONST store, 
          # then add the CONST **idx** to the opcode arguments.
          ci <- cb$putconst(as.name(args[[inst_arg_idx]]))
          bc_args[[bc_arg_idx]] <- ci
          inst_arg_idx <- inst_arg_idx + 1L
        } else if (arg_type == 'label') {
          label <- args[[inst_arg_idx]]
          bc_args[[bc_arg_idx]] <- label
          inst_arg_idx <- inst_arg_idx + 1L
        } else if (arg_type == 'math1name') {
          idx <- which(compiler:::math1funs == args[[inst_arg_idx]])
          if (length(idx) == 0) {
            stop(sprintf("[%i] no such math1 function: '%s'", i, args[[inst_arg_idx]]))
          }
          bc_args[[bc_arg_idx]] <- idx - 1L # zero-indexed reference to math1funs
          inst_arg_idx <- inst_arg_idx + 1L
        } else {
          stop(sprintf("[%i] arg %i type not understood: '%s'", i, bc_arg_idx, arg_type))
        }
      } # end loop over all args
      
      # Add this op + args to the bytecode buffer
      do.call(cb$putcode, c(op$code, bc_args))
      
    } # Standard OP
    
    i <- i + 1L
  } # Next row
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if last command is RETURN.
  # This applies to the main code as well nested code in MAKEPROM
  # and MAKECLOSURE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  last_op <- utils::tail(suppressWarnings(cb$code()), 1)
  if (!last_op %in% c(rbytecode::ops$RETURN$code, rbytecode::ops$RETURNJMP$code)) {
    print(bcdf)
    row <- bcdf[nrow(bcdf),]
    msg <- sprintf("Depth=%i, Line=%i, PC=%i, OP=%s\nExpected RETURN/RETURNJMP after this instruction",
                   row$depth, row$line, row$pc, row$op)
    stop(msg)
  }  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the code buffer to a bytecode object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bytecode <- compiler:::codeBufCode(cb, cntxt) 
  
  bytecode
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile bytecode assembly into an R bytecode object
#' 
#' @param code R bytecode assembly code as a single string
#' @inheritParams compile_bcdf
#' 
#' @examples
#' \dontrun{
#' code <- "LDCONST 1\nLDCONST 2\nADD\nRETURN"
#' bc <- asm(code)
#' eval(bc) # 3
#' }
#' 
#' 
#' @return bytecode object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
asm <- function(code, expr_default = quote(stop("[rbytecode]"))) {
 bcdf <- parse_code(code)
 compile_bcdf(bcdf, expr_default = expr_default)
}


