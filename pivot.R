pp <- function(Close, High, Low) (Close + High + Low) / 3
b1 <- function(Close, High, Low) 2 * pp(Close, High, Low) - High
s1 <- function(Close, High, Low) 2 * pp(Close, High, Low) - Low
b2 <- function(Close, High, Low) pp(Close, High, Low) - High + Low
s2 <- function(Close, High, Low) pp(Close, High, Low) + High - Low
lb <- function(Close, High, Low) b1(Close, High, Low) - High + Low
hb <- function(Close, High, Low) s1(Close, High, Low) + High - Low
PIVOT <- function(Close, High, Low) {
  c(
    HBOP = hb(Close, High, Low),
    S2   = s2(Close, High, Low),
    S1   = s1(Close, High, Low),
    P    = pp(Close, High, Low),
    B1   = b1(Close, High, Low),
    B2   = b2(Close, High, Low),
    LBOP = lb(Close, High, Low)
  )
}