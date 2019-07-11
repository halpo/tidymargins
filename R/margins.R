#' @importFrom magrittr %>% %T>%
#' @import dplyr
#' @importFrom pkgcond assert_that %<<% %<<<%
#' @importFrom assertthat is.flag is.string
NULL
globalVariables('.')


#' Operate over margins
#'
#' Alter a function to operate over groups and all possible
#' subsets of groupings, including no grouping.
#'
#' @param FUN the function to wrap.
#' @param all.name the string to use to represent that a variable was marginalized over.
#'
#' @export
#' @example inst/examples/ex-with_margins.R
with_margins <-
function( FUN #< Function to compute one groups and margins.
        , all.name = "(All)"
        ){
    function(.data, ...){
        fix_not_present <-
            function(vars_not_present){
                if (length(vars_not_present) == 0) return(rlang::exprs())
                if (inherits(.data, 'tbl_lazy')) {
                    vars_not_present %>%
                        rlang::rep_along(all.name) %>%
                        rlang::set_names(vars_not_present)
                } else {
                    vars_not_present %>%
                        purrr::map(~
                            if (is.factor(pull(.data, .))){
                                old.levels <- levels(pull(.data, .))
                                assert_that(!(all.name %in% old.levels))
                                new.levels <- c(all.name, old.levels)
                                rlang::expr(
                                    factor( all.name
                                          , levels = !!new.levels
                                          , ordered = !!is.ordered(pull(.data, .))
                                          )
                                )
                            } else
                                all.name
                        ) %>% rlang::set_names(vars_not_present)
                }
            }
        fix_present <-
            function(vars){
                if (length(vars) == 0 ) return(rlang::exprs())
                if (inherits(.data, 'tbl_lazy')) {
                    rlang::exprs()
                } else {
                    vars %>% rlang::syms() %>% purrr::map(~rlang::expr(
                        if (is.factor(!!.))
                            forcats::fct_expand(!!., all.name) %>%
                            forcats::fct_relevel(all.name)
                        else
                            !!.
                    )) %>% rlang::set_names(vars)
                }
            }
        g   <- group_vars(.data)
        com <- Reduce(c,lapply(seq(0, length(g)), utils::combn, x=g, simplify=FALSE))
        com <- com[order(desc(sapply(com, length)))]
        lapply(com, function(x, .data){
            vars_present  <- intersect(g,x)
            vars_not_present <- setdiff(g,x)
            FUN(group_by(ungroup(.data), !!!rlang::syms(x), add=FALSE), ...) %T>%
                { assert_that( !any(tbl_vars(.) %in% as.character(setdiff(g, x)))
                             , msg = "Unexpected grouping variable in output of" %<<%
                                      deparse(substitute(FUN))
                             )
                } %>%
            mutate(!!!fix_not_present(vars_not_present)) %>%
            ungroup() %>%
            mutate(!!!fix_present(vars_present))
        }, .data) %>%
        purrr::reduce(dplyr::union_all)
    }
}
if(F){#@example
    x <- c( 'a', 'b', 'c')
    y <- c( 'd', 'e', 'f')
    data <- expand.grid( x = x
                       , y = y
                       , .rep = 1:10
                       ) %>%
            mutate( v = rnorm(90)) %>%
            select(-.rep)


    ms <- with_margins(summarise)
    expect_is(ms, "function")
    ms(group_by(data, x, y), N=n(), sum=sum(v))
}
if(F){#@testing
    x <- c( 'a', 'b', 'c')
    y <- c( 'd', 'e', 'f')
    data <- expand.grid( x = x
                       , y = y
                       , .rep = 1:10
                       ) %>%
            mutate( v = rnorm(90)) %>%
            select(-.rep)


    ms <- with_margins(summarise)
    expect_is(ms, "function")
    val <- ms(group_by(data, x, y), N=n(), sum=sum(v))

    expect_identical( val[1:3]
                    , data.frame( x = c(rep(x, each =length(y)), x, rep("(All)", length(y)+1))
                                , y = c(rep(y, length(x)), rep("(All)", length(x)), y, "(All)")
                                , N = c( rep(10L, length(x) * length(y))
                                       , rep(30L, length(x) + length(y))
                                       , 90L
                                       )
                                ) %>% as_tibble
                    )
}

