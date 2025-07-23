## The raw list uses alist() so that each element is not evaluated immediately,
## which allows us to generate nice names with add_names_to_alist()
raw_alist <- alist(
    ## numerical scalars
    NULL,
    NA,
    0L,
    1.2,

    ## numerical vectors
    integer(),
    numeric(),
    -1:3,
    rnorm(3),

    ## logicals
    logical(),
    TRUE,
    c(TRUE, NA, FALSE),

    ## characters
    character(),
    "",
    letters,
    c("a", NA, "b"),

    ## data frames
    data.frame(),
    data.frame(a = NA),
    data.frame(a = letters),
    data.frame(a = 1:10, b = NA),
    iris[0, ],
    iris[, 0],
    iris[1, , drop = FALSE],
    iris[, 1, drop = FALSE],

    ## matrices
    matrix(),
    matrix(0, 0, 0),
    matrix(1, 1, 0),
    matrix(1, 0, 1),

    ## lists
    list()
)

input_list <- add_names_to_alist(raw_alist)
