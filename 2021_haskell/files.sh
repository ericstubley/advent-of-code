# run me with zsh! not sh, the behaviour is different
# make the files in each directory
# for the future, how could you have done this on the command line?
# think about that
year=2021
for label in {01..25}
do
    mkdir $label
    cd $label
    id="$year"_"$label"
    touch goal_"$id".txt
    # cp ../makefile .
    cp ../puzzle_template.hs puzzle_"$id".hs
    cp ../test_template.hs test_"$id".hs
    cp ../main_template.hs main_"$id".hs
    sed -i '' s/XX/"$label"/g puzzle_"$id".hs
    sed -i '' s/XX/"$label"/g test_"$id".hs
    sed -i '' s/XX/"$label"/g main_"$id".hs
    # sed -i '' s/XX/"$label"/g makefile 
    cd ..
done