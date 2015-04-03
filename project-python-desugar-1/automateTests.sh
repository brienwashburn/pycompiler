#!/bin/bash 
if [ $1 = "-f" ]; then
		./run -f $2 > testAutomationOurFile
		./rdesugar -f $2 > testAutomationRefFile
		comm -3 testAutomationOurFile testAutomationRefFile
elif [ $1 = "-fd" ]; then
		./run -f $2 > testAutomationOurFile
		./rdesugar -f $2 > testAutomationRefFile
		diff -EbwBay --strip-trailing-cr testAutomationOurFile testAutomationRefFile
elif [ $1 = "-d" ]; then
		FILES=$2/*

		for f in $FILES
		do
			echo "$f"
			./run -f $f > testAutomationOurFile
			./rdesugar -f $f > testAutomationRefFile
			diff -EbwBay --strip-trailing-cr testAutomationOurFile testAutomationRefFile
		done
else
		FILES=$1/*

		for f in $FILES
		do
			echo "$f"
			./run -f $f > testAutomationOurFile
			./rdesugar -f $f > testAutomationRefFile
		        comm -3 testAutomationOurFile testAutomationRefFile	
		done
fi




