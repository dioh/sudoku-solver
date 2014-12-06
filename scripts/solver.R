require('GenSA')
sudoku = read.csv('~/proj/sudoku-solver/testdata/sudoku1', header=FALSE)
delta = function (x) { length(x) - length(unique(x)) } 
evaluation = function(mat){ 
  colum = apply(mat, MARGIN=1, delta)
  rows = apply(mat, MARGIN=2, delta)
  return(sum(colum + rows))
}
fixed_cells = which(sudoku == 0, arr.ind = T)

is_fixed = function(i_j){
  any(apply(fixed_cells, 1, function(x) all(i_j == x)))
}


gen_neighbourgh = function(mat){
  N2 = dim(mat)[1]
  N = sqrt(N2)
  i_j = sample(1:N2, 2)
  l_k = get_next_index(i_j)
  # print('cambio')
  # print(c(i_j))
  # print('por')
  # print(c(l_k))
  if(all(i_j == l_k) | is_fixed(i_j)){
    return(gen_neighbourgh(mat))
  }
  tmp = mat[i_j[1], i_j[2]]
  mat[i_j[1], i_j[2]] = mat[l_k[1], l_k[2]]
  mat[l_k[1], l_k[2]] = tmp
  return(mat)
}

get_next_index = function(i_j){ 
    # 1, 2, 3 => 1
    # 4, 5, 6 => 4
    # 7, 8, 9 => 7

    N = 3
    replaces = c(1,1,1,4,4,4,7,7,7)

    ranges_start = c(replaces[i_j[1]], replaces[i_j[2]])
    l = sample(seq(from=ranges_start[1], length.out = N ), 1)
    k = sample(seq(from=ranges_start[2], length.out = N ), 1)
    return(c(l,k))
}

random_zero_replace_asign = function(mat){
  N2 = dim(mat)[1]
  N=sqrt(N2)
  
  for(i in c(1,4,7)){
    for (j in c(1,4,7)){
      square = mat[seq(i,i+(N-1)),seq(j,j+(N-1))]
      used_nums = square[square>0]
      all_nums = 1:N2
      fillers = all_nums[!all_nums %in% used_nums]
      square[square == 0] = sample(fillers, length(fillers))
      mat[seq(i,i+(N-1)),seq(j,j+(N-1))] = square
    }
  }
  return(mat)
}
# objte


eval_next = function(mat) {
  nextt= gen_neighbourgh(matrix(mat, 9, 9))
  # # print(dim(nextt))
  eval = evaluation(nextt)
  return(eval)
}
fixed_cells = which(sudoku == 0, arr.ind = T)
sudoku = apply(sudoku, 1:2, as.double)
sudoku = random_zero_replace_asign(sudoku)

assign('sudoku', random_zero_replace_asign(sudoku), .GlobalEnv)
#GenSA(par=c(1,1), lower=c(1,1), upper=c(9,9), fn = gen_neighbourgh) 
# GenSA(par=random_zero_replace_asign(sudoku),
#       fn=eval_next, lower = rep(1, 81), upper = rep(9, 81))

# soluciones = data.frame(size=0, sol = -1)

# for (i in 1:50){
# print(i)
#   sol = LSopt(evaluation,
#         list(x0=sudoku,
#              neighbour = gen_neighbourgh, printDetail = F, printBar=F,
#              nS = 100* i))$OFvalue
#   soluciones = rbind(soluciones, cbind(size = (i * 100), sol=sol))
# }

# soluciones.TS = data.frame(size=0, sol = -1)

# for (i in 1:50){
#   print(i)
#   sol = TAopt(evaluation,
#               list(x0=sudoku,
#                    neighbour = gen_neighbourgh, printDetail = F, printBar=F,
#                    nS = 100* i))$OFvalue
#   soluciones.TS = rbind(soluciones.TS, cbind(size = (i * 100), sol=sol))
# }

qplot(x=size, y = sol, data=soluciones[2:51,], geom=c('point', 'smooth')) + xlab('#iteraciones') +
  ylab('Función de Costo')
# ggsave('~/proj/sudoku-solver/latex/imgs/LSol_progresion.png')

qplot(x=size, y = sol, data=soluciones.TS[2:51,], geom=c('point', 'smooth')) +
 xlab('#iteraciones') +
  ylab('Función de Costo')
# ggsave('~/proj/sudoku-solver/latex/imgs/TASol_progresion.png')

sudoku_sol = read.csv('~/proj/sudoku-solver/testdata/sudoku_sol', header=FALSE)

evaluation(sudoku_sol)


remove_random = function(sudoku_sol){
  i_j = sample(1: 9, 2)
  sudoku_sol[i_j[1], i_j[2]] = 0
  return(sudoku_sol) 
}
