#!/bin/bash

#Разрешили glob-ы в которых нет ни одного файла, но команда не выполняется
shopt -s nullglob

QPATH=$HOME/.nyaqueue

#Создание директории без выведения ошибок
mkdir -p "$QPATH"
mkdir -p "$QPATH/requests"
mkdir -p "$QPATH/queue"

#Создаём временный файл и записываем туда запросы а потом отправляем в главную папку запросов
for a in "$@"
do
    r=`mktemp --tmpdir="$QPATH/queue"`
    echo "$a" > "$r"
    mv "$r" "$QPATH/requests"
done

#Если не существует такого файла, создаём его
if [[ ! -f "$QPATH/running" ]]; then
    touch "$QPATH/running" 
    trap "rm '$QPATH/running'" 0

    (while [ 1 ]
    do
        trap "fork()" SIGHUP
        for a in "$QPATH/requests"/*
        do
            url=`cat "$a"`
            rm "$a"
            wget -c "$url"
        done
	
        sleep 1
    done) & disown
fi
