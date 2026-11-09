# library(rlang)

#
# Helpers
#

#' Creates a targeted quosure whose environment contains only the needed symbols
#'
#' We want to keep only relevant names in our quosure environment to
#' prevent memory leaks, especially as streams can refer several
#' levels deep. So we search for symbol references in the expression
#' and put the *locals* from the caller's environment into the new
#' frame. Because the caller's parent is our parent, we avoid an
#' environment with too heavy a burden of extra variables, but we
#' can still find them by the ordinary search chain when evaluating.
#'
#' @importFrom rlang enquo quo_get_expr quo_get_env new_environment sym
#' @importFrom rlang env_has env_get env_poke quo_set_env
#' @keywords internal
lightweight_enquo <- function(arg) {
  quo0 <- enquo(arg)
  expr <- quo_get_expr(quo0)
  cenv <- quo_get_env(quo0)  # This is the caller's active execution frame!

  find_symbols <- function(e) {
    if( is.symbol(e) ) return(as.character(e))
    if( is.call(e) )   return(unique(unlist(lapply(as.list(e), find_symbols))))
    return(character(0))
  }
  needed_vars <- find_symbols(expr)
  purged_env <- new_environment(parent = env_parent(cenv))  # skip the caller's environment

  for (var in needed_vars) {
      if( env_has(cenv, var) ) {
          # Keep only the explicitly used variables from the caller's frame
          env_poke(purged_env, var, env_get(cenv, var))
      }
  }
  return( quo_set_env(quo0, purged_env) )
}


#
# Lazy Computations
#

#' Creates a delayed (lazy) computation
#'
#' @importFrom rlang enquo quo_get_expr quo_get_env is_quosure
#' @importFrom rlang env_poke env_get is_symbol eval_tidy
#' @export
delay <- function(x) {
    raw_quo <- enquo(x)  # x is unevaluated here
    expr    <- quo_get_expr(raw_quo)

    # If x's expression was a quosure, we want to use that. This happens
    # when passing a quosure from above, see cons.
    # Alternatively, x is a real expression and we process it.
    if( is_quosure(expr) ) {
        quo <- eval_tidy(raw_quo)  # extract the quosure from the promise wrapper
    } else {
        quo <- lightweight_enquo(!!raw_quo)  # unquote then enquo
    }

    env <- quo_get_env(quo)

    # Private cache in our clean environment to store the computed value
    NOTHING  <- sym("NOTHING") # Unique sentinel object
    env_poke(env, ".cached_value", NOTHING)

    function() {
        # Check if the calculation has already been computed
        cached <- env_get(env, ".cached_value")

        if( is_symbol(cached, "NOTHING") ) {
            # Not yet computed, cache and return
            res <- eval_tidy(quo)
            env_poke(env, ".cached_value", res)
            return(res)
        }
        return(cached)
    }
}


#' A proxy for waldo::compare to prevent forcing the recursive/lazy stream tail
#'
#' @importFrom waldo compare_proxy
#' @export
compare_proxy.stream <- function(x, path) {
    if (is_empty(x)) {
        return(list(object = "EMPTY STREAM", path = path))
    }

    # Flatten the stream node into a dead, primitive string vector.
    # This tells waldo: "Do not crawl the components, just show this text".
    representation <- paste0("<stream [head: ", head(x), "] :: [tail promise]>")
    list(object = representation, path = paste0("as.character(", path, ")"))
}

