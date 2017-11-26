library(tidyverse)

A <- matrix(
  c(
    1, 1, 1, 1, 1, # A, B, C, D, E
    0, 1, 1, 0, 1, # B, C, E
    1, 1, 0, 1, 1, # A, B, D, E
    1, 0, 0, 1, 1, # A, D, E
    1, 1, 1, 0, 0, # A. B. C
    0, 1, 0, 1, 0, # B, D
    1, 0, 1, 0, 1, # A, C, E
    1, 1, 1, 0, 1, # A, B, C, E
    1, 0, 1, 0, 0  # A, C
  ), 
  ncol = 5, 
  byrow = TRUE
)

input_data <- 
  A %>% 
  t() %>% 
  as_tibble %>% 
  transmute(
    hbp        = V1 %>% as.logical(),
    motrin     = V2 %>% as.logical(),
    bad_review = V3 %>% as.logical(),
    post_2017  = V4 %>% as.logical(),
    lethargic  = V5 %>% as.logical(),
    pre_2017   = V6 %>% as.logical(),
    died       = V7 %>% as.logical(),
    radiation  = V8 %>% as.logical(),
    nausea     = V9 %>% as.logical()
  ) %>% 
  mutate(
    patientId  = c(
      "555-555-5555",
      "453-029-2939",
      "382-029-9273",
      "382-574-2848",
      "342-485-3828"
    )
  ) %>% 
  select(patientId, everything()) 


library(lpSolve)

c <-
  c(
    1, 1, 1, 1, 1
  )


A <- matrix(
  c(
    1, 1, 1, 1, 1, # A, B, C, D, E
    0, 1, 1, 0, 1, # B, C, E
    1, 1, 0, 1, 1, # A, B, D, E
    1, 0, 0, 1, 1, # A, D, E
    1, 1, 1, 0, 0, # A. B. C
    1, 0, 0, 1, 0, # B, D
    1, 0, 1, 0, 1, # A, C, E
    1, 1, 1, 0, 1, # A, B, C, E
    1, 0, 1, 0, 0  # A, C
  ), 
  ncol = 5, 
  byrow = TRUE
)


b = 1 %>% 
  rep(9)

dir <- 
  ">=" %>% 
  rep(9)

s = lp("min", c, A, dir, b, all.bin = TRUE)

s$solution


# Nurse scheduling problem

# http://www.iosrjournals.org/iosr-jnhs/papers/vol3-issue6/Version-1/E03612428.pdf

A <- 
  matrix(
    c(
      1, 1, 0, 0, 0, 0,
      0, 1, 1, 0, 0, 0, 
      0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0,
      0, 0, 0, 0, 1, 1,
      1, 0, 0, 0, 0, 1,
      1, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 1
    ),
    ncol = 6,
    byrow = TRUE
  )


b <- 
  c(
    70, 
    140,
    200,
    85,
    25, 
    40,
    0,
    0,
    0,
    0,
    0,
    0
  )

c <- 
  1 %>% 
  rep(6)

dir <- 
  ">=" %>% 
  rep(12)

s = lp("min", c, A, dir, b, all.int = TRUE)


cbind(A,dir, b) %>% 
  noquote
