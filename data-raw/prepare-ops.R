
suppressPackageStartupMessages({
  library(dplyr)
})

tsv <- here::here("data-raw/bytecode.tsv")
ops <- read.delim(tsv, sep = "\t") %>%
  as_tibble()

ops <- ops %>%
  arrange(code) %>%
  mutate(
    idx      = code + 1L,
    fullname = name,
    name     = sub("\\.OP$", "", name)
  ) %>%
  select(name, fullname, everything())

ops_df <- ops


# Turn the argument types into a list.
# remove any NA or blanks
ops$args <- apply(
  ops[,c('arg1', 'arg2', 'arg3', 'arg4')], 1, 
  function(x) {
    res <- unname(unlist(x))
    res <- res[!is.na(res) & res != '']
    res
  }
)

ops <- ops %>% select(-arg1, -arg2, -arg3, -arg4)



ops$arg_descs <- apply(
  ops[,c('desc1', 'desc2', 'desc3', 'desc4')], 1, 
  function(x) {
    res <- unname(unlist(x))
    res <- res[!is.na(res) & res != '']
    res
  }
)

ops <- ops %>% select(-desc1, -desc2, -desc3, -desc4)



ops$has_expridx <- vapply(ops$args, \(x) 'expridx' %in% x, logical(1))


ops <- setNames(purrr::transpose(ops), ops$name)

usethis::use_data(ops, internal = FALSE, overwrite = TRUE)



usethis::use_data(ops_df, internal = TRUE, overwrite = TRUE)