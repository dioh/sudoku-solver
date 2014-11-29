sudoku = read.csv('testdata/sudoku1', header=FALSE)
delta = function (x) { length(x) - length(unique(x)) + 1}
evaluation = function(mat){ apply(mat, 1, delta) + apply(mat, 0, delta) }

gen_neighbourgh = function(mat){
    N2 = length(mat)
    N = sqrt(length(mat))
    # get i, j
    i_j = sample(1:N2, 2)
    # obtengo el subcuadrado de i_j
    # ranges_start = ((i_j %% N) * 3) + 1
    ranges_start = ((i_j %/% N) * N  ) 
    ranges_start[ranges_start == 0] = 1
    ranges_start[ranges_start == N2] = N2 - N
    print(ranges_start)
    l = sample(seq(from=ranges_start[1], length.out = N), 1)
    k = sample(seq(from=ranges_start[2], length.out = N), 1)
    print( paste(i_j))
    print(paste('[', l, ', ', k, ']'))
    if(mat[i_j[1], i_j[2]] == mat[l, k])
        return(gen_neighbourgh(mat))
    tmp = mat[i_j[1], i_j[2]]
    mat[i_j[1], i_j[2]] = mat[l, k]
    mat[l, k] = tmp
    print(paste('Cambiando [', i_j[0], i_j[2], '] por [', l, k, ']') )
    return (mat)

}
# objte
