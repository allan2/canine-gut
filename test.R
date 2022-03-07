add <- function(a, f) {
    a + f(3)
}

add_one <- function(a) {
    a + 1
}

add(5, add_one)