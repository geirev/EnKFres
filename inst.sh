#!/bin/ksh
export ENKFRESDIR=/home/geve/Dropbox/EnKFres
export CASEDIR=/home/geve/TULLBALL
mkdir $CASEDIR
cd $ENKFRESDIR
cp -r Code $CASEDIR
cd $CASEDIR/Code
ln -sf $ENKFRESDIR/SOURCE/*.F* .
ln -sf include/*.inc .
cp $ENKFRESDIR/LSOURCE/*.F* .
#cp $ENKFRESDIR/bin/*  $HOME/bin
make
