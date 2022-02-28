# make the files in each directory
# for the future, how could you have done this on the command line?
# think about that
for label in {01..25}
do
    cd $label
    rm goal.txt
    touch goal_2017_"$label".txt
    cp ../puzzle_template.py puzzle_2017_"$label".py
    cp ../test_template.py test_2017_"$label".py
    sed -i '' s/XX/"$label"/g test_2017_"$label".py
    cd ..
done