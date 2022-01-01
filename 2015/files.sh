# make the files in each directory
# for the future, how could you have done this on the command line?
# think about that
for label in {02..25}
do
    cd $label
    mv "$label"a.py puzzle_2015_"$label"a.py
    mv "$label"b.py puzzle_2015_"$label"b.py
    cp ../test_template.py test.py
    chmod 755 test.py
    cd ..
done