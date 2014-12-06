require('GenSA')
sudoku = read.csv('~/proj/sudoku-solver/testdata/sudoku1', header=FALSE)
delta = function (x) { length(x) - length(unique(x)) + 1}
evaluation = function(mat){ sum(apply(mat, MARGIN=1, delta) + apply(mat, MARGIN=2, delta)) }

gen_neighbourgh = function(fixed_cells){
  print(fixed_cells)
    mat = sudoku
    for (repeats in seq(1,fixed_cells)) {
        N2 = dim(mat)[1]
        N = sqrt(N2)
        i_j = sample(1:N2, 2)
        l_k = get_next_index(i_j)
        # print('cambio')
        # print(c(i_j))
        # print('por')
        # print(c(l_k))
        if(all(i_j == l_k))
            gen_neighbourgh(1)
        tmp = mat[i_j[1], i_j[2]]
        mat[i_j[1], i_j[2]] = mat[l_k[1], l_k[2]]
        mat[l_k[1], l_k[2]] = tmp
        assign("sudoku", mat, envir = .GlobalEnv)
    }
    return (evaluation(mat))
}
get_next_index = function(i_j){ 
    # 1, 2, 3 => 1
    # 4, 5, 6 => 4
    # 7, 8, 9 => 7

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

 sudoku = apply(sudoku, 1:2, as.double)
# GenSA(par=random_zero_replace_asign(sudoku),
#       fn=eval_next, lower = rep(1, 81), upper = rep(9, 81))
