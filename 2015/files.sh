# make the files in each directory
# for the future, how could you have done this on the command line?
# think about that
for label in {01..25}
do
    cd $label
    cp ../template.py "$label"a.py
    cp ../template.py "$label"b.py
    chmod 755 "$label"a.py
    chmod 755 "$label"b.py
    cd ..
done