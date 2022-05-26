# run me with zsh! not sh, the behaviour is different
# make the files in each directory
# for the future, how could you have done this on the command line?
# think about that
for label in {01..25}
do
    cd $label
    rm goal.txt
    touch goal_2020_"$label".txt
    # cp ../makefile .
    cp ../puzzle_template.hs puzzle_2020_"$label".hs
    cp ../test_template.hs test_2020_"$label".hs
    cp ../main_template.hs main_2020_"$label".hs
    sed -i '' s/XX/"$label"/g puzzle_2020_"$label".hs
    sed -i '' s/XX/"$label"/g test_2020_"$label".hs
    sed -i '' s/XX/"$label"/g main_2020_"$label".hs
    # sed -i '' s/XX/"$label"/g makefile 
    cd ..
done