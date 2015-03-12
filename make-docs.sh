#! /bin/sh -xe
mkdir -p build/docs

cp docs/*.png build/docs

for P in `ls docs/*.md`; do 
    grip $P --gfm --export build/docs/$(basename $P).html
    sed -i "s/.md/.md.html/" build/docs/$(basename $P).html
done