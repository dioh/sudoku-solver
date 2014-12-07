require('GenSA')
sudoku = read.csv('~/proj/sudoku-solver/testdata/sudoku1', header=FALSE)
delta = function (x) { length(x) - length(unique(x)) } 

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

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

soluciones.TS = data.frame(size=0, sol = -1)
actual.sudoku = sudoku
for (i in 1:50){
  print(i)
      sol = TAopt(evaluation,
                    list(x0=actual.sudoku,
                         neighbour = gen_neighbourgh, printDetail = F, printBar=F,
                         nS = i*100))$OFvalue
  soluciones.TS = rbind(soluciones.TS, cbind(size = (i * 100), sol=sol))
}

es.estable.TS = data.frame()

for (i in 1:10){
    for(j in 1:50){
        sol = TAopt(evaluation,
                    list(x0=actual.sudoku,
                         neighbour = gen_neighbourgh, printDetail = F, printBar=F,
                         nS = i * 100))$OFvalue
        es.estable = rbind(es.estable, cbind(id = i * 100, trial =j, sol=sol))
    }
}

es.estable.TA = data.frame()

for (i in 1:10){
  for(j in 1:50){
    sol = TAopt(evaluation,
                list(x0=actual.sudoku,
                     neighbour = gen_neighbourgh, printDetail = F, printBar=F,
                     nS = i * 100))$OFvalue
    es.estable.TA = rbind(es.estable.TA, cbind(id = i * 300, trial =j, sol=sol))
  }
}

es.estable.BL = data.frame()

for (i in 1:10){
  for(j in 1:50){
    sol = LSopt(evaluation,
                list(x0=actual.sudoku,
                     neighbour = gen_neighbourgh, printDetail = F, printBar=F,
                     nS = i * 100))$OFvalue
    es.estable.BL = rbind(es.estable.BL, cbind(id = i * 300, trial =j, sol=sol))
  }
}

stats.es.estable.BL = aggregate(sol ~ id, data=es.estable.BL, mean)
stats.es.estable.BL = cbind(stats.es.estable.BL,sd=
                            aggregate(sol ~ id, data=es.estable.BL, sd)$sol)

stats.es.estable.TA = aggregate(sol ~ id, data=es.estable.TA, mean)
stats.es.estable.TA = cbind(stats.es.estable.TA,sd=
                            aggregate(sol ~ id, data=es.estable.TA, sd)$sol)

ggplot(data = stats.es.estable.BL,
       aes(x = id, y = sol, ymax=sol + sd, ymin = sol-sd)) + 
  geom_point() + geom_smooth() +
  geom_errorbar() + xlab('#iteraciones') +
  ylab('Función de Costo')
ggsave('~/proj/sudoku-solver/latex/imgs/BLsol_progresion.png')

ggplot(data = stats.es.estable.TA,
       aes(x = id, y = sol, ymax=sol + sd, ymin = sol-sd)) + 
  geom_point() + geom_smooth() + 
  geom_errorbar() + xlab('#iteraciones') +
  ylab('Función de Costo')
ggsave('~/proj/sudoku-solver/latex/imgs/TAsol_progresion.png')

gqgplot(x=size, y = sol, data=soluciones[2:51,], geom=c('point', 'smooth')) + xlab('#iteraciones') +
  ylab('Función de Costo')
ggsave('~/proj/sudoku-solver/latex/imgs/LSol_progresion_qchico.png')

qplot(x=size, y = sol, data=soluciones.TS[2:51,], geom=c('point', 'smooth')) +
 xlab('#iteraciones') +
  ylab('Función de Costo')
ggsave('~/proj/sudoku-solver/latex/imgs/TASol_progresion.png')

sudoku_sol = read.csv('~/proj/sudoku-solver/testdata/sudoku_sol', header=FALSE)

evaluation(sudoku_sol)


remove_random = function(sudoku_sol){
  i_j = sample(1: 9, 2)
  sudoku_sol[i_j[1], i_j[2]] = 0
  return(sudoku_sol) 
}

# pruebas_easy = data.frame()
# sudokus = data.frame()
# 
# for (i in 1:20){
#   sudokuto = read.csv(paste('~/proj/sudoku-solver/testdata/sudoku_easy_', i,  '.csv', sep=''),
#                       header=FALSE)
#   sudokus = rbind(sudokus, cbind(id=i, sudokuto))
# }
# 
# resueltos.sudokus = data.frame()
# 
# for (i in 1:20){
#   actual.sudoku = subset(sudokus, id==i, select = 2:10)
#   fixed_cells = which(actual.sudoku == 0, arr.ind = T)
#   actual.sudoku = apply(actual.sudoku, 1:2, as.double)
#   actual.sudoku = random_zero_replace_asign(actual.sudoku)
#   
#   TASol = TAopt(evaluation,
#                 list(x0=actual.sudoku,
#                      neighbour = gen_neighbourgh, printDetail = F, printBar=F,
#                      nS = 3000,
#                      q=0.1))
#   LSSol = LSopt(evaluation, list(x0=actual.sudoku, printDetail = F, printBar=F, 
#                                  neighbour = gen_neighbourgh,
#                                  nS = 3000))
#   resueltos.sudokus = rbind(resueltos.sudokus, cbind(id=i, type='TA', TASol$OFvalue))
#   resueltos.sudokus = rbind(resueltos.sudokus, cbind(id=i, type='LS', LSSol$OFvalue))
#   
# } 
# sudokus.med = data.frame()
# 
# for (i in 1:20){
#   sudokuto = read.csv(paste('~/proj/sudoku-solver/testdata/sudoku_med_', i,  '.csv', sep=''),
#                       header=FALSE)
#   sudokus.med = rbind(sudokus.med, cbind(id=i, sudokuto))
# }
# 
# resueltos.sudokus.med = data.frame()
# 
# for (i in 1:20){
#   actual.sudoku = subset(sudokus.med, id==i, select = 2:10)
#   fixed_cells = which(actual.sudoku == 0, arr.ind = T)
#   actual.sudoku = apply(actual.sudoku, 1:2, as.double)
#   actual.sudoku = random_zero_replace_asign(actual.sudoku)
#   
#   TASol = TAopt(evaluation,
#                 list(x0=actual.sudoku,
#                      neighbour = gen_neighbourgh, printDetail = F, printBar=F,
#                      nS = 3000,
#                      q=0.1))
#   LSSol = LSopt(evaluation, list(x0=actual.sudoku, printDetail = F, printBar=F, 
#                                  neighbour = gen_neighbourgh,
#                                  nS = 3000))
#   resueltos.sudokus.med = rbind(resueltos.sudokus.med, cbind(id=i, type='TA', TASol$OFvalue))
#   resueltos.sudokus.med = rbind(resueltos.sudokus.med, cbind(id=i, type='LS', LSSol$OFvalue))
#   
# }
# 
# 
# sudokus.hard = data.frame()
# 
# for (i in 1:20){
#   sudokuto = read.csv(paste('~/proj/sudoku-solver/testdata/sudoku_hard_', i,  '.csv', sep=''),
#                       header=FALSE)
#   sudokus.hard = rbind(sudokus.hard, cbind(id=i, sudokuto))
# }

# resueltos.sudokus.hard = data.frame()
# 
# for (i in 1:20){
#   actual.sudoku = subset(sudokus.hard, id==i, select = 2:10)
#   fixed_cells = which(actual.sudoku == 0, arr.ind = T)
#   actual.sudoku = apply(actual.sudoku, 1:2, as.double)
#   actual.sudoku = random_zero_replace_asign(actual.sudoku)
#   
#   TASol = TAopt(evaluation,
#                 list(x0=actual.sudoku,
#                      neighbour = gen_neighbourgh, printDetail = F, printBar=F,
#                      nS = 3000,
#                      q=0.1))
#   LSSol = LSopt(evaluation, list(x0=actual.sudoku, printDetail = F, printBar=F, 
#                                  neighbour = gen_neighbourgh,
#                                  nS = 3000))
#   resueltos.sudokus.hard = rbind(resueltos.sudokus.hard, cbind(id=i, type='TA', TASol$OFvalue))
#   resueltos.sudokus.hard = rbind(resueltos.sudokus.hard, cbind(id=i, type='LS', LSSol$OFvalue))
#   
# }
# 

qplot(V3, data=resueltos.sudokus, geom='histogram', 
      fill=type, binwidth=1) + scale_x_discrete(limits=c(0,2:8)) + 
  xlab('#Objetivo Final') + ylab('Cantidad') +
  guides(fill=guide_legend(title='Tipo')) 
ggsave('~/proj/sudoku-solver/latex/imgs/problemas_easy_histo.png')


qplot(V3, data=resueltos.sudokus.med, geom='histogram', 
      fill=type, binwidth=1) + scale_x_discrete(limits=c(0,2:8)) + 
  xlab('#Objetivo Final') + ylab('Cantidad') +
  guides(fill=guide_legend(title='Tipo')) 
ggsave('~/proj/sudoku-solver/latex/imgs/problemas_med_histo.png')


qplot(V3, data=resueltos.sudokus.hard, geom='histogram', 
      fill=type, binwidth=1) + scale_x_discrete(limits=c(0,2:8)) + 
  xlab('#Objetivo Final') + ylab('Cantidad') +
  guides(fill=guide_legend(title='Tipo')) 
ggsave('~/proj/sudoku-solver/latex/imgs/problemas_hard_histo.png')


