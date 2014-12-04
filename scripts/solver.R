require('GenSA')
sudoku = read.csv('~/proj/sudoku-solver/testdata/sudoku1', header=FALSE)
delta = function (x) { length(x) - length(unique(x)) + 1}
evaluation = function(mat){ sum(apply(mat, MARGIN=1, delta) + apply(mat, MARGIN=2, delta)) }

gen_neighbourgh = function(mat){
  print(as.list(match.call()))
    print('entre')
    print(dim(mat))
    N2 = dim(mat)[1]
    print(paste('N2', as.character( N2)))
    N = sqrt(N2)
    # get i, j
    i_j = sample(1:N2, 2)
    print('random ij')
    print(i_j)
    # obtengo el subcuadrado de i_j
    # ranges_start = ((i_j %% N) * 3) + 1
    ranges_start = ((i_j %/% N) * N  ) 
    print('sub cuadrante')
    print(ranges_start)
    ranges_start = replace(ranges_start,  ranges_start==0, 1)
    ranges_start <- replace(ranges_start,  ranges_start==N2, N2 - N) 
    print('ranges =')
    print(ranges_start)
    l = sample(seq(from=ranges_start[1], length.out = N), 1)
    k = sample(seq(from=ranges_start[2], length.out = N), 1)
    print( i_j)
    print(paste('[', l, ', ', k, ']'))
    if(mat[i_j[1], i_j[2]] == mat[l, k])
        return(gen_neighbourgh(mat))
    tmp = mat[i_j[1], i_j[2]]
    mat[i_j[1], i_j[2]] = mat[l, k]
    mat[l, k] = tmp
    #     print(paste('Cambiando [', i_j[0], i_j[2], '] por [', l, k, ']') )
    return (mat)
}

random_zero_replace_asign = function(mat){
  N2 = dim(mat)[1]
  ran_no_cero = apply(
    mat, MARGIN = c(1,2),
    function(x) ifelse(x==0, sample(1:N2, 1), x))
  return(ran_no_cero)
}
# objte


eval_next = function(mat) {
  nextt= gen_neighbourgh(matrix(mat, 9, 9))
  print(dim(nextt))
  eval = evaluation(nextt)
  return(eval)
}

sudoku = apply(sudoku, 1:2, as.double)
GenSA(par=random_zero_replace_asign(sudoku), fn=eval_next, lower = rep(1, 81), upper = rep(9, 81))
