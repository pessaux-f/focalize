#!/bin/sh
xsltproc /usr/local/lib/focalize/focdoc2html.xsl $1 | xsltproc /usr/local/lib/focalize/mmlctop2_0.xsl - > $1.xml
