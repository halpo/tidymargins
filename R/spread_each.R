#' Spread multiple variables
#'
#' This is a multiple variable version of the function
#' [tidyr::spread()].
#'
#' @inheritParams tidyr::spread
#' @param ... the columns to act as the values to spread out.
#' @param .   The separator between the key levels and the value column names.
#' @param sep the character to use to separate parts of column names.
#' @param key.first If `TRUE`, the default, the columns are named
#'                  `{key level}{sep}{value column name}`,
#'                  otherwise the format is `{value column name}{sep}{key level}{sep}`
#'
#' @return A wide [`tbl_df`][tibble::tbl_df], with multiple value columns spread out.
#'
#' @seealso
#' * [Wide versus long data](https://en.wikipedia.org/wiki/Wide_and_narrow_data) (also known as narrow data) on Wikipedia.
#' * [tidyr::spread()] for the single variable version.
#' @export
#' @example inst/examples/ex-spread_each.R
spread_each <-
function( data                          #< A tibble or compatible object.
        , key                           #< key to be used as for the 'super-columns'
        , ...                           #< Value variables to be spread
        , fill=NA                       #< a single value or a named list of values to fill in structural missing.
        , convert=FALSE                 #< See <spread>
        , drop=FALSE                    #< See <spread>
        , sep='.'                       #< the separator to be used to separate the names of the super- and sub-columns.
        , key.first=TRUE
        ){
    key <- rlang::enquo(key)
    dots <- rlang::quos(...)
    assert_that( is.flag(convert)
               , is.flag(drop)
               , is.string(sep)
               )
    key.var <- tidyselect::vars_pull(tbl_vars(data), !!key)
    value.cols      <- tidyselect::vars_select(tbl_vars(data), !!!dots)
    retained.groups <- dplyr::group_vars(data) %>% setdiff(key.var)
    grouping.cols   <- tbl_vars(data) %>% setdiff(key.var) %>% setdiff(value.cols)

    assert_that(rlang::is_dictionaryish(value.cols))
    if (!is.null(names(fill))) {
        if (all(. <- rlang::have_name(fill))) {
            assert_that( rlang::is_dictionaryish(fill)
                       , all(names(value.cols) %in% names(fill))
                       )
        } else {
            assert_that(sum(!.)==1L, msg='`fill` should have only one default/unnamed value')
            fill <- fill[match(value.cols, names(fill), nomatch = which(!.))] %>%
                rlang::set_names(value.cols)
        }
    } else {
        fill <- rlang::rep_along(value.cols, fill) %>% rlang::set_names(value.cols)
    }

    key.levels <- levels2(pull(data, key.var))

    f <- function(col, name){
        new.names <-
            rlang::set_names(key.levels
                            , if (key.first) paste(key.levels, name, sep=sep)
                                        else paste(name, key.levels, sep=sep))

        data %>% dplyr::ungroup() %>%
            dplyr::select( key.var, col, grouping.cols) %>%
            tidyr::spread( key   = key.var
                         , value = col
                         , fill  = fill[[name]]
                         , sep   = NULL
                         ) %>%
            dplyr::rename(., !!!(new.names[new.names %in% names(.)]))
    }
    col.order <- purrr::map(if (key.first) key.levels
                                      else names(rlang::quos_auto_name(dots))
                           , ~rlang::expr(starts_with(!!.)))
    value.cols %>%
        purrr::imap(f) %>%
        purrr::reduce(full_join, by=grouping.cols) %>%
        dplyr::select( !!!grouping.cols
                     , !!!(col.order)
                     ) %>%
        dplyr::group_by(!!!rlang::syms(retained.groups))
}
if(F){#@example
data <- expand.grid( x = c( 'a', 'b', 'c')
                    , y = c( 'd', 'e', 'f')
                    , .rep = 1:10
                    ) %>%
         mutate( v = rnorm(90)) %>%
         select(-.rep)
long <- summarise(group_by(data, x, y),N=n(), sum=sum(v))

spread_each(long, y, N, sum)
}
if(F){#@testing
    data <- expand.grid( x = c( 'a', 'b', 'c')
                        , y = c( 'd', 'e', 'f')
                        , .rep = 1:10
                        ) %>%
             mutate( v = rep(c(-1, 0, 1), length.out=90)) %>%
             select(-.rep)
    long <- data %>%
        group_by(x, y) %>%
        summarise(N=n(), sum=sum(v))
    val <- spread_each(long, y, N, sum)

    expect_equal(dim(val), c(3L,7L))
    expect_equal(names(val), c('x', 'd.N', 'd.sum', 'e.N', 'e.sum'
                              , 'f.N', 'f.sum'))

    val2 <- spread_each(long, y, N, sum, key.first=FALSE)
    expect_equal(dim(val2), c(3L,7L))
    expect_equal(names(val2), c('x'
                               , paste0('N', '.', c( 'd', 'e', 'f'))
                               , paste0('sum', '.', c( 'd', 'e', 'f'))
                               ))
}
if(FALSE){#@testing spread_each(fill=...)
    data <- expand.grid( x = c( 'a', 'b', 'c')
                       , y = c( 'd', 'e', 'f')
                       , .rep = 1:10
                       ) %>%
             mutate( v = rep(c(-1, 0, 1), length.out=90)) %>%
             select(-.rep)
    long <- data %>%
        group_by(x, y) %>%
        summarise(N=n(), sum=sum(v)) %>%
        filter(!(x=='b' & y=='e'))

    val <- spread_each(long, y, N, sum, fill=list(N='#N/A', sum='???'))
    expect_is(val, 'tbl')
    expect_equal( val[2,c('e.N', 'e.sum')]
                , tibble(e.N = '#N/A', e.sum = '???')
                )

    expect_error(spread_each(long, y, N, sum, fill=list(N='#N/A')))
    expect_error(spread_each(long, y, N, sum, fill=list('#N/A', '???', x='.')))

    val2 <- spread_each(long, y, N, sum, fill=list(N='#N/A', '???'))
    expect_is(val2, 'tbl')
    expect_equal( val2[2,c('e.N', 'e.sum')]
                , tibble(e.N = '#N/A', e.sum = '???')
                )
}


levels2 <- function(x){
    if (inherits(x, 'factor'))
        return(levels(x))
    else if (inherits(x, 'character'))
        return(sort(unique(x)))
    else
        return(sort(unique(as.character(x))))
}
if(F){#@testing
    x <- ordered(c('b', 'a', 'c'), levels=c('c', 'b', 'a'))
    expect_equal(levels2(x), levels(x))

    x <- c('c', 'b', 'a')
    expect_equal(levels2(x), c('a', 'b', 'c'))

    x <- 1:3
    expect_equal(levels2(x), c('1','2','3'))
}

