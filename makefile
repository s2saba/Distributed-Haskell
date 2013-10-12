


clean :
	-@find . -iname '*.hi' -exec rm \{\} \;
	-@find . -iname '*.o' -exec rm \{\} \;
