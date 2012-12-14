FCLOBJS = ex_implications.vo or_id_com_good.vo or_id_com_shortest.vo same_comps.vo diff_comps.vo signal2.vo signal3.vo controller/controller_algo.vo controller/controller_complete.vo controller/controller1.vo controller/controller2.vo controller/controller3.vo controller/controller5.vo controller/controller5.vo lemmas1.vo lemmas3.vo lemmas4.vo lemmas5.vo

TEXSRCS = conclusion.tex first_order_logic.tex intro.tex main.tex init.tex	program.tex


%.eps:%.fig
	fig2dev -L eps $< $@

%.vo:%.fcl
	focalizec $<

all: eps code pdf

pdf: $(TEXSRCS)
	pdflatex main.tex

eps:

code: $(FCLOBJS)

clean: clean-code clean-latex

clean-code:
	rm -f *.ml *.fo *.cm* *.v *.vo *.pfc *.zvtmp.pfc *.glob *.zv *.log *.aux
	(cd controller &&\
	rm -f *.ml *.fo *.cm* *.v *.vo *.pfc *.zvtmp.pfc *.glob *.zv *.log *.aux &&\
	cd ..)

clean-latex:
	rm -f main.dvi main.pdf
