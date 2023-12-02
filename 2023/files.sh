# run me with zsh! not sh, the behaviour is different
# make the files in each directory
year=2023
for label in {01..25}
do
    mkdir $label
    cd $label
    id="$year"_"$label"
    touch goal_"$id".txt
    cp ../puzzle_template.hs puzzle_"$id".hs
    cp ../test_template.hs test_"$id".hs
    cp ../main_template.hs main_"$id".hs
    sed -i '' s/XX/"$label"/g puzzle_"$id".hs
    sed -i '' s/XX/"$label"/g test_"$id".hs
    sed -i '' s/XX/"$label"/g main_"$id".hs
    cd ..
done