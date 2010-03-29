all:	saikoro.tex saikoro.c
	gcc -o saikoro -lm saikoro.c
	pdftex saikoro.tex

%.tex:	%.w
	cweave $<

%.c:	%.w
	ctangle $<

