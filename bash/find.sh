#!/bin/bash


type=0
template=*

find()
{
for file in *; do
if [[ $file == $template ]]; then
if [ $file != "*" ]; then
case $type in
0) echo "$1$file"
;;
1) if [ -f $file ]; then
echo "$1$file"
fi
;;
2) if [ -d $file ]; then
echo "$1$file"
fi
;;
3) if ! [ -f $file ] && ! [ -d $file ]; then
echo "$1$file"
fi
;;
esac

if test -d $file 
then
cd $file
find "$1$file/"
cd ..
fi
fi
fi
done
}

madeType()
{
case "$1" in
"f") type=1
;;
"d") type=2
;;
"l") type=3
;;
*) echo "error"
exit
;;
esac
}

if [ "$#" -eq 0 ]; then
echo "."
find "./"
exit
fi

if [ "$#" -eq 2 ] && [ $1 = "-type" ]; then
madeType $2
if [ $type -eq 2 ]; then
echo "."
fi
find "./"
exit
fi 

if [ "$#" -eq 2 ] && [ $1 = "-iname" ]; then
template=$2
if [[ "." == template ]]; then
echo "."
fi
find "./"
exit
fi

if [ "$#" -eq 2 ]; then
echo "error"
exit
fi

if [ "$#" -eq 4 ] && [ $1 = "-iname" ] && [ $3 = "-type" ]; then
template=$2
madeType $4
if [[ "." == template ]] && [ $type -eq 2 ]; then
echo "."
fi
find "./"
exit
fi

if [ "$#" -eq 4 ] && [ $3 = "-iname" ] && [ $1 = "-type" ]; then
template=$4
madeType $2
if [[ "." == template ]] && [ $type -eq 2 ]; then
echo "."
fi
find "./"
exit
fi

echo "error"
