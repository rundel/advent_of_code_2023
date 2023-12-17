
#Initialize auxiliary arrays
dist = matrix(Inf, nrow(m), ncol(m))
dist[1,1]=0

origin = map(seq_len(nrow(m)), ~list())
visited = matrix(0, nrow(m), ncol(m)) 
prev = matrix(10, nrow(m), ncol(m))

x = 1
y = 1

get_prev = function(x,y) {
  origin[[x]][[y]] %||% c(x-1,y-1)
}

get_dir = function(x,y) {
  rbind(
    c(x,y),
    purrr::possibly(get_prev, c(0,0))(x,y)
  ) |>
    apply(2,diff)
}


 
as_dir = function(x) {
  case_when(
    all(x == c(1,0))  ~ "N",
    all(x == c(-1,0)) ~ "S",
    all(x == c(0,1))  ~ "W",
    all(x == c(0,-1)) ~ "E",
    TRUE              ~ "*"
  )
}



#Loop Dijkstra until reaching the target cell
repeat {
  #cat(x,y,"\n")
  # move to x+1,y
  
  p1 = purrr::possibly(get_prev, c(0,0))(x,y) 
  p2 = purrr::possibly(get_prev, c(-1,-1))(p1[1],p1[2]) 
  p3 = purrr::possibly(get_prev, c(-2,-2))(p2[1],p2[2]) 
  
  steps = rbind(c(x,y),p1,p2,p3) |> apply(2,diff) |> apply(1, as_dir)
  
  if (x < nrow(m)) {
    prop = 4-length(unique(c("S", steps)))
    if (dist[x+1,y] >= m[x+1,y]+dist[x,y] & !visited[x+1,y] & prev[x+1,y] > prop & prop < 3) {
      dist[x+1,y] = m[x+1,y] + dist[x,y]
      prev[x+1,y] = prop
      origin[[x+1]][[y]] = c(x, y)
    }
  }
  # move to x-1,y
  if (x > 1) {
    prop = 4-length(unique(c("N",steps)))
    if (dist[x-1,y] > m[x-1,y] + dist[x,y] & !visited[x-1,y] & prev[x-1,y] > prop & prop < 3) {
      dist[x-1,y] = m[x-1,y]+dist[x,y]
      origin[[x-1]][[y]] = c(x, y)
    }
  }
  # move to x,y+1
  if (y < ncol(m)) {
    prop = 4-length(unique(c("E",steps)))
    if (dist[x,y+1] > m[x,y+1]+dist[x,y] & !visited[x,y+1] & prev[x,y+1] > prop & prop < 3) {
      dist[x,y+1] = m[x,y+1]+dist[x,y]
      origin[[x]][[y+1]] = c(x, y)
    }
  }
  # move to x,y-1
  if (y > 1) {
    prop = 4-length(unique(c("W",steps)))
    if (dist[x,y-1] > m[x,y-1] + dist[x,y] & !visited[x,y-1] & prev[x,y-1] > prop & prop < 3) {
      dist[x,y-1] = m[x,y-1]+dist[x,y]
      origin[[x]][[y-1]] = c(x, y)
    }
  }

  visited[x,y]=TRUE
  dismaptemp = dist
  dismaptemp[which(visited == 1)] = Inf
  
  # now we find the shortest path so far
  minpost = which(dismaptemp == min(dismaptemp), arr.ind = TRUE)
    
  x = minpost[1,1]
  y = minpost[1,2]
  
  if (x==nrow(m) & y==ncol(m))
    break
}

#Start backtracking to plot the path  
x = nrow(m)
y = ncol(m)
path=list()

repeat {
  path = append(path, list(c(x,y)))
  cat(x,y,"-> ")
  prev = origin[[x]][[y]]
  x = prev[1] |> unlist()
  y = prev[2] |> unlist()
  
  cat(x,y,"\n")
  if (x == 1 & y == 1)
    break
}
path = append(path, list(c(x,y)))

path |> do.call(rbind, args=_) |> plot()

