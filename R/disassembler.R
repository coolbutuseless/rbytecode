
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the R object a disassembled bytecode object?
#' 
#' @param x R object
#'
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_disassembled_bytecode <- function(x) {
  is.list(x) && identical(x[[1]], as.name(".Code"))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Insert the given row into a data.frame 
#'
#' @param df data.frame
#' @param before index at which to insert the new row
#' @param row list or data.frame.  Must match all column names in order.
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_insert_row <- function(df, before, row) {
  
  if (!identical(names(row), names(df))) {
    print(row)
    cat("names(row): ", deparse1(names(row)), "\n")
    cat("names(df) : ", deparse1(names(df )), "\n")
  }
  
  if (before < 1 || before > nrow(df)) {
    stop("df_insert_row(): Out of range.")
  }
  
  pre <- df[seq_len(before - 1),]
  post <- df[seq(before, nrow(df)),]
  rbind(pre, as.data.frame(row), post)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a row to a data.frame
#' 
#' @param df data.frame
#' @param row new row to add. all names must match.
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_add_row <- function(df, row) {
  df[nrow(df) + 1L, ] <- row
  df
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Disassemble R object to a code list object
#' 
#' Note: have to use the \code{.internal(disassemble)} such that raw integers
#'       are returned rather than OPCODE character names
#'
#' @param x R object
#' \itemize{
#'   \item{A \code{language} object. e.g. \code{quote(1 + 1)}}
#'   \item{An already compiled \code{bytecode} object}
#'   \item{A Function}
#' }
#'
#' @return A code list object. This is the standard form of bytecode disassembled
#'         with \code{.Internal(disassemble(x))}
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dis_raw <- function(x) {
  
  # If the user passed in a quoted language object, then compile it first
  if (typeof(x) == 'language') {
    x <- compiler::compile(x)
  }
  
  # Disassemble the bytecode - compile first if necessary
  # 'd' = the disassembled 'code list' object (a list)
  if (inherits(x, 'bytecode')) {
    d <- .Internal(disassemble(x))
  } else if (is_disassembled_bytecode(x)) {
    # Already disassembled code
    # If it was disassembled with 'compiler::disassemble()' then 
    # need to replace opnames with opcodes in the code list.
    if (is.list(x[[2]])) {
      d <- x
      opcodes <- as.character(d[[2]])
      opcodes <- sub(".OP$", "", opcodes) # remote .OP suffix
      opcodes <- ifelse(
        opcodes %in% names(rbytecode::ops),
        match(opcodes, names(rbytecode::ops)) - 1,
        opcodes
      )
      opcodes <- as.integer(opcodes)
      d[[2]] <- opcodes
    } else {
      d <- x 
    }
  } else if (typeof(x) == 'closure') {
    # This is a function. 
    # Grab the bytecode for the body of this function
    b <- .Internal(bodyCode(x))
    if (!inherits(b, 'bytecode')) {
      # If there's no bytecode associated with this function, it 
      # means it is not compiled!
      # So compile it ...
      x <- compiler::cmpfun(x)
      b <- .Internal(bodyCode(x))
    }
    d <- .Internal(disassemble(b))
  } else {
    stop("'dis()': argument type not handled: ", class(x))
  }
  
  d
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Disassemble R object to a code list object
#' @param x expr. Will be quoted.
#' @return code list
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
disq_raw <- function(x) {
  dis_raw(substitute(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a label in the given environment
#'
#' @param env an environment with an \code{lcount} variable to keep track 
#'        of the global label count.  This variable is used to generate
#'        globally unique labels
#'
#' @return returns a globally unique character label (starting with '@') and
#'         updates the label count within the environment
#'         
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_label <- function(env) {
  env$lcount <- env$lcount + 1L
  paste0("@label", env$lcount)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Disassemble R objects to a bytecode data.frame 
#' 
#' @param x Possible inputs: language object, call object, compiled bytecode, 
#'          functionquoted expression.
#' @param incl_expr include the expression index in the output. Default: FALSE.
#'        This is an advanced feature
#' @param depth internal variable tracking recursion depth. Not usually set by user.
#' @param env environment for keeping track of labels. Not usually set by user.
#' 
#' @return bytecode data.frame (\code{bcdf})
#' @import compiler
#' @export
#'
#' @examples
#' \dontrun{
#' dis(quote(1 + x))
#' fc <- compiler::cmpfun(\(x) {2 * x + 1}) 
#' dis(fc)
#' }
#' 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dis <- function(x, incl_expr = FALSE, depth = 0L, env = NULL) {
  
  if (is.null(env)) {
    # NULL environment should only happen at depth=-
    stopifnot(depth == 0) 
    env <- as.environment(list(lcount = 0L))
  }
  
  # Create an empty data.frame
  df <- data.frame(
    depth  = integer(),
    pc     = integer(),
    opcode = integer(),
    op     = character()
  )
  df$args <- list()
  if (incl_expr) {
    df$expr <- list()
  }
  
  # Get disassembled bytecode as a code list object
  d <- dis_raw(x)
  
  # Grab the constants i.e. list of values, names, call objects
  # Convert to a character representation
  consts <- d[[3]]

  # The second element in the code list object
  # instructions = opcodes and arguments
  insts <- d[[2]]
  
  # First opcode is the bytecode version number.
  # R4.3.1   version = 12
  # Check and remove.
  stopifnot(insts[1] == 12) 
  insts <- insts[-1]
  
  # Program counter
  pc  <- 1L
  
  # Keep track of label usage
  labels <- list()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Process each instruction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while (pc <= length(insts)) {
    
    # Next instruction must be the an opcode
    opcode <- insts[[pc]]
    
    # get the information about this opcode
    op <- rbytecode::ops[[opcode + 1L]]
    
    # initiate the df row for this instruction
    row <- list(depth = depth, pc = pc, opcode = opcode, op = op$name)
    
    # increment program counter
    pc <- pc + 1L
    
    # Keep track if any of the arguments are recursive i.e. MAKEPROM, MAKECLOSURE
    recurse <- FALSE
    
    # Accumulate the arguments for this op
    args_out <- list()
    expr     <- list(NULL)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Process each argument to this op
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (j in seq_len(op$argc)) {
      
      # get the value of this argument
      arg_code <- insts[[pc]]
      
      
      if (op$name == 'MAKECLOSURE') {
        # The arg is a pointer to the consts
        # The vconst alue represents a closure list with
        # list(formal_args, closure-body-as-bytecode, srcref)
        arg <- consts[[arg_code + 1L]]
        formal_args <- arg[[1]]

        if (is.null(formal_args)) {
          # There are no formal arguments to this closure
          args_out <- list(NULL)
        } else {
          # Unpack the pairlist to semicolon-delimited character stirng
          # i.e. pairlist(alist(x=, y = 9))  =>  "x; y = 9"
          varnames    <- names(formal_args)
          values      <- as.character(formal_args)
          formal_args <- ifelse(values == "", varnames, paste(varnames, '=', values))
          formal_args <- paste(formal_args, collapse = "; ")
          args_out    <- c(args_out, formal_args)
        }
      } else if (op$name == 'MAKEPROM') {
        # A promise doesn't have arguments in this intermediate representation
        args_out <- list(NULL)
      }
      
      if (op$name %in% c('MAKECLOSURE', 'MAKEPROM')) {
        recurse <- TRUE
        
        # Begin recurse 
        # by first putting the MAKE* op in the bytecode data.frame
        row$args <- args_out
        if (incl_expr) row$expr <- expr
        df <- df_add_row(df, row)
        
        # For a promise, the arg_code points to an element in the 'consts' list
        # which is a bytecode object.
        arg <- consts[[arg_code + 1L]]
        if (op$name == 'MAKECLOSURE') {
          # If we are working with a closure, then the closure-body is a bytecode object
          # closure arg = list(formal_args, closure-body-as-bytecode, srcref)
          arg <- arg[[2]]  # just the bytecode
        }
        
        # Disassemble the bytecode that makes up the closure or promise
        df_inner <- dis(arg, incl_expr = incl_expr, depth = depth + 1L, env = env)
        
        # Include the promise/closure body in the final intermediate representation
        df <- rbind(df, df_inner)
        
        # End recurse and insert a marker for ENDMAKECOSURE, ENDMAKEPROM
        # Note: these two ops aren't real ops, but without them there is no
        # way for a human to easily delimit the extents of a promise or closure
        # when writing bytecode assembly.
        if (op$name == "MAKECLOSURE") {
          row      <- list(depth = depth, pc = NA, opcode = NA, op = "ENDMAKECLOSURE")
          row$args <- list(NULL)
          if (incl_expr) row$expr <- list(NULL)
          df       <- df_add_row(df, row)
        } else if (op$name == "MAKEPROM") {
          row      <- list(depth = depth, pc = NA, opcode = NA, op = "ENDMAKEPROM")
          row$args <- list(NULL)
          if (incl_expr) row$expr <- list(NULL)
          df       <- df_add_row(df, row)
        } 
      } else {
        # Handle all other ops (which aren't MAKEPROM or MAKECLOSURE)
        switch(
          op$args[[j]],
          
          expridx = {
            if (incl_expr) {
              expr <- list(consts[[arg_code + 1L]])
            }
          },
          
          label = {
            # label_out <- gen_label(env)
            # args_out <- c(args_out, label_out)
            # labels[[label_out]] <- arg_code
            
            label_pc <- arg_code
            if (label_pc %in% unlist(labels)) {
              # there is already a label pointing here. 
              # don't create a new label. re-use the existing one.
              idx <- which(label_pc == unlist(labels))[[1]]
              this_label <- names(labels)[[idx]]
            } else {
              this_label <- gen_label(env)
              labels[[this_label]] <- label_pc
            }
            args_out <- c(args_out, this_label)
          },
          
          value = {
            val <- arg_code
            args_out <- c(args_out, val)
          },
          
          name = {
            val <- as.character(consts[[arg_code + 1L]])
            args_out <- c(args_out, val)
          },
          
          const = ,
          asis  = {
            val <- consts[[arg_code + 1L]]
            if (!is.complex(val)) {
              val <- deparse1(val) |> str2lang()
            }
            args_out <- c(args_out, list(val))
          },
          
          math1name = {
            math1fun <- compiler:::math1funs[arg_code + 1L]
            args_out <- c(args_out, math1fun)
          },
          
          char_vec = {
            # For SWITCH
            val <- consts[[arg_code + 1L]]
            args_out <- c(args_out, list(val))
          },
          
          labels = {
            # For SWITCH
            val <- consts[[arg_code + 1L]]
            if (is.null(val)) {
              # No labels given here. 
              args_out <- c(args_out, list(NULL))
            } else {
              this_labels <- c()
              for (label_pc in val) {
                if (label_pc %in% unlist(labels)) {
                  # there is already a label pointing here. 
                  # don't create a new label. re-use the existing one.
                  idx <- which(label_pc == unlist(labels))[[1]]
                  this_label <- names(labels)[[idx]]
                } else {
                  this_label <- gen_label(env)
                  labels[[this_label]] <- label_pc
                }
                this_labels <- c(this_labels, this_label)
              }              
              args_out <- c(args_out, list(this_labels))
            }
          },
          
          # Default
          {
            warning(">>: Unhandled: ", op$name, " ", op$args[[j]]) 
          }
        )
      }
      pc <- pc + 1L
    } # for j in seq_len(op$argc)
    
    
    if (!recurse) {
      # Add args to df row object
      # print(args_out)
      if (length(args_out) == 0) {
        row$args <- list(NULL)
      } else {
        row$args <- list(args_out)
      }
      if (incl_expr) row$expr <- expr
      df <- df_add_row(df, row)
    }
  } # while(pc <= length(insts))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Insert labels
  # labels = list(label_name = pc, label_name = pc, ...)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(labels)) {
    # Extract the PC and name for this label
    pc      <- labels[[i]]
    label   <- names(labels)[i]
    
    # Work out which row in the bytecode data.frame has this PC
    row_idx <- which(df$pc == pc)
    
    # Sanity check. There should be a PC which exactly matches the label
    # reference. If there isn't, then there's something deeply wrong.
    stopifnot(!is.null(row_idx), length(row_idx) == 1, !is.na(row_idx))
    
    # Create a bytecode data.frame row for the label.
    row <- data.frame(
      depth  = df$depth[row_idx],
      pc     = NA_integer_,
      opcode = NA_integer_,
      op     = label
    )
    row$args <- list(NULL)
    
    if (isTRUE(incl_expr)) {
      row$expr <- list(NULL)
    }
    
    # insert label row in data.frame
    df <- df_insert_row(df, row_idx, row)
  }
  
  class(df) <- c('bcdf', class(df))
  rownames(df) <- NULL
  df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A wrapper for \code{dis()} which accepts unquoted expressions.
#'
#' @param x unquoted expression
#' @inheritParams dis
#'
#' @examples
#' \dontrun{
#' disq(1 + 3)
#' dis(quote(1 + 3))
#' }
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
disq <- function(x, incl_expr = FALSE) {
  dis(substitute(x), incl_expr = incl_expr)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a bytecode data.frame
#' 
#' @param x Character representation of a bytecode data.frame
#' @param ... arguments passed to \code{cat()}
#' 
#' @examples
#' \dontrun{
#' bcdf <- disq(1 + x)
#' txt  <- as.character(bcdf)
#' txt
#' }
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.bcdftxt <- function(x, ...) {
  cat(x, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a bytecode data.frame to a text representation
#' 
#' @param x bytecode data.frame produced by \code{dis()}, \code{disq()} 
#'        and \code{parse_code()}
#' @param incl_expr Include the stored expression?  Default: FALSE
#' @param ... ignored
#'
#' @examples
#' \dontrun{
#' as.character(disq(1 + 1))
#' }
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.bcdf <- function(x, incl_expr = FALSE, ...) {
  txt <- lapply(seq_len(nrow(x)), \(idx) as_character_op(bcdf = x, idx = idx, incl_expr = incl_expr))
  
  res <- paste(txt, collapse = "\n")
  class(res) <- 'bcdftxt'
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single row of a bytecode data.frame to a string
#'
#' @param bcdf bytecode data.frame
#' @param idx index of row to print.
#' @inheritParams as.character.bcdf
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_character_op <- function(bcdf, idx, incl_expr = FALSE) {
  row <- bcdf[idx,]
  op  <- rbytecode::ops[[row$op]]
  
  op_name <- row$op
  if (row$depth == 0) {
    indent <- NULL
  } else {
    indent  <- paste(rep(" ", 2 * row$depth - 1L), collapse = "")
  }
  
  if (is.null(op)) {
    # Labels, ENDMAKEPROM, ENDMAKECLOSURE
    op_args <- NULL
  } else if (op$name == "MAKEPROM") {
    op_args <- NULL
  } else if (op$name == 'MAKECLOSURE') {
    op_args <- row$args[[1]]
  } else {
    args    <- row$args[[1]]
    op_args <- c()
    arg_idx <- 1L
    for (i in seq_len(op$argc)) {
      arg_type <- op$args[[i]]
      
      this_arg <- switch(
        arg_type,
        expridx = NULL,
        const   = {
          res <- deparse1(args[[arg_idx]])
          arg_idx <- arg_idx + 1L
          res
        },
        
        char_vec = {
          res <- str2lang(deparse1(args[[arg_idx]]))
          # print(res)
          arg_idx <- arg_idx + 1L
          res
        },
        
        labels = {
          res <- deparse1(args[[arg_idx]])
          # print(res)
          arg_idx <- arg_idx + 1L
          res
        },
        
        # default 
        {
          res <- args[[arg_idx]]
          arg_idx <- arg_idx + 1L
          res
        }
      )
      
      op_args <- c(op_args, this_arg)
    }
  }
  
  if (row$op == 'SWITCH') {
    # print(op_args)
    op_args <- paste(op_args, collapse = "; ")
  } else {
    op_args <- paste(op_args, collapse = " ")
  }

  res <- paste(c(
    indent, 
    op_name,
    # sprintf("%-14s", op_name),
    op_args
  ), collapse = " ", sep = "")
  
  if (isTRUE(incl_expr) && !is.null(row$expr[[1]])) {
    res <- paste(res, "$$", deparse1(row$expr[[1]]))
  }
  
  
  res
}


