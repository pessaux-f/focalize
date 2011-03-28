
default:
	@echo To transform a .xml file to a .html file type:
	@echo make path/my_file.html where path/my_file.xml exists

all: example.html

clean:

cleanall:

cleanxml:

%.html:%.xml test_report.xslt
	xsltproc test_report.xslt $<  >$@
