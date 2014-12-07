for i in {1..20}; do
    qqwing --generate 1 --compact --difficulty expert | sed "s/./&,/g;s/\./0/g;s/,$//" > ../testdata/sudoku_hard_$i.csv;
done
