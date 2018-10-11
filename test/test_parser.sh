#!/bin/bash

for f in parser/*.bt; do
	echo -n "$f: "
	../btp < $f
done

