library(tidyverse)

## Part 1

d = read_lines(here::here("day24/test.txt"))
lb = 7
ub = 27

d = read_lines(here::here("day24/input.txt"))
lb = 200000000000000
ub = 400000000000000

df = str_replace(d, " @ ", ", ") |>
  str_split(", ") |>
  map(as.numeric) |>
  do.call(rbind, args=_) |>
  as.data.frame() |>
  set_names(c("x","y","z","dx","dy","dz")) |>
  as_tibble() |>
  rowwise() |>
  transmute(
    p1 = list(c(x,y,z)),
    p2 = list(c(x+dx,y+dy,z+dz)),
    v = list(c(dx,dy,dz))
  )

future::plan(future::multisession, workers=8)

z = expand_grid(x = 1:nrow(df), y = 1:nrow(df)) |>
  filter(x < y) |>
  mutate(
    res = furrr::future_map2(
      x, y, 
      function(x,y) { # Weird precision issue
        x1 = df$p1[[x]][[1]] |> Rmpfr::mpfr(128) 
        x2 = df$p2[[x]][[1]] |> Rmpfr::mpfr(128)
        x3 = df$p1[[y]][[1]] |> Rmpfr::mpfr(128)
        x4 = df$p2[[y]][[1]] |> Rmpfr::mpfr(128)
        y1 = df$p1[[x]][[2]] |> Rmpfr::mpfr(128)
        y2 = df$p2[[x]][[2]] |> Rmpfr::mpfr(128)
        y3 = df$p1[[y]][[2]] |> Rmpfr::mpfr(128)
        y4 = df$p2[[y]][[2]] |> Rmpfr::mpfr(128)
        
        x_v1 = df$v[[x]][[1]]
        x_v2 = df$v[[y]][[1]]
        y_v1 = df$v[[x]][[2]]
        y_v2 = df$v[[y]][[2]]
        
        Px = ( (x1*y2 - y1*x2)*(x3-x4)-(x1-x2)*(x3*y4 - y3*x4) ) /
          ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)) |> as.numeric()
        Py = ( (x1*y2 - y1*x2)*(y3-y4)-(y1-y2)*(x3*y4 - y3*x4) ) /
          ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)) |> as.numeric()
        
        t = c(t1=(Px - x1) / x_v1,
              t2=(Px - x3) / x_v2,
              t3=(Py - y1) / y_v1,
              t4=(Py - y3) / y_v2) |>
          as.numeric()
        
        future = (sign(Px - x1) == sign(x_v1)) & 
          (sign(Px - x3) == sign(x_v2)) &
          (sign(Py - y1) == sign(y_v1)) &
          (sign(Py - y3) == sign(y_v2))
        
        list(
          Px=as.numeric(Px), Py=as.numeric(Py), t=t, future = future
        )
      },
      .progress = TRUE
    )
  ) |>
  unnest_wider(res) |>
  mutate(
    test_area = (Px >= lb & Px <= ub &
      Py > lb & Py < ub),
    intersects = future & test_area
  )
  
z |> 
  pull(intersects) |>
  sum()



## Part 2

d = read_lines(here::here("day24/test.txt"))
d = read_lines(here::here("day24/input.txt"))

df = str_replace(d, " @ ", ", ") |>
  str_split(", ") |>
  map(as.numeric) |>
  do.call(rbind, args=_) |>
  as.data.frame() |>
  set_names(c("x","y","z","dx","dy","dz")) |>
  as_tibble() |>
  rowwise() |>
  transmute(
    p1 = list(c(x,y,z)),
    p2 = list(c(x+dx,y+dy,z+dz)),
    v = list(c(dx,dy,dz))
  )


F = function(vals) {
  x = vals[1] 
  y = vals[2] 
  z = vals[3] 
  vx = vals[4]
  vy = vals[5]
  vz = vals[6]
  
  res = mpfr(rep(0,6), 128)
  for (i in 1:3) {
    p = df$p1[[i]]
    v = df$v[[i]]
    
    res[2*(i-1)+1] = (x-p[1])*(vy-v[2]) - (y-p[2])*(vx-v[1])
    res[2*(i-1)+2] = (x-p[1])*(vz-v[3]) - (z-p[3])*(vx-v[1])
  }
  res
}



## Based on pracma::jacobian
jacobian = function (f, x0, heps = .Machine$double.eps^(1/3)) {
  n <- length(x0)
  m <- length(f(x0))
  jacob <- matrix(NA, m, n) |> Rmpfr::mpfr(128)
  hh <- numeric(n)
  for (i in 1:n) {
    hh[i] <- heps
    jacob[, i] <- (f(x0 + hh) - f(x0 - hh))/(2 * heps)
    hh[i] <- 0
  }
  return(jacob)
}

inv = function (a) {
  if (length(a) == 0) 
    return(matrix(0, nrow = 0, ncol = 0))
  
  a = matrix(as.numeric(a), nrow(a), ncol(a))
  
  e <- try(b <- solve(a), silent = TRUE)
  if (inherits(e, "try-error")) {
    warning("Matrix appears to be singular.")
    b <- rep(Inf, length(a)) |> Rmpfr::mpfr(128)
    dim(b) <- dim(a)
  }
  return(b |> Rmpfr::mpfr(128))
}

## Based on pracma::broyden
broyden = function (F, x0, J0 = NULL, maxiter = 100, tol = .Machine$double.eps^(1/2)) {
  y0 <- F(x0)
  if (length(x0) != length(y0)) 
    stop("Function 'F' must be 'square', i.e. from R^n to R^n .")
  if (length(x0) == 1) 
    stop("Function 'F' must not be a univariate function.")
  if (is.null(J0)) {
    A0 <- jacobian(F, x0)
  }
  else {
    A0 <- J0
  }
  B0 <- inv(A0)
  if (any(is.infinite(B0))) 
    B0 <- diag(length(x0))
  xnew <- x0 - B0 %*% y0
  ynew <- F(xnew)
  k <- 1
  while (k < maxiter) {
    s <- xnew - x0
    d <- ynew - y0
    if (norm(s, "F") < tol || norm(as.matrix(ynew), "F") < tol) 
      break
    B0 <- B0 + (s - B0 %*% d) %*% t(s) %*% B0/c(t(s) %*% B0 %*% d)
    x0 <- xnew
    xnew <- xnew - B0 %*% ynew
    y0 <- ynew
    ynew <- F(xnew)
    k <- k + 1
  }
  if (k >= maxiter) 
    warning(paste("Not converged: Max number of iterations reached."))
  fnew <- sqrt(sum(ynew^2))
  return(list(zero = c(xnew), fnorm = fnew, niter = k))
}

broyden(F, c(df$p1[[4]], df$v[[4]]) |> Rmpfr::mpfr(128), maxiter = 1000 )$zero[1:3] |> round(0) |> sum()

