<?xml version="1.0" encoding="utf-8"?>
<!-- FoCDoc to LaTeX. Very experimental -->
<!-- $Id: focdoc2tex.xsl,v 1.1 2009-01-12 17:14:39 pessaux Exp $ -->
<!--important: le xmlns="http://www.w3.org/1999/xhtml" a cet endroit et pas apres-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns="http://www.w3.org/1999/xhtml"
                version="1.0">

<xsl:import href="proposition2tex.xsl"/>
<xsl:output method="text" encoding="ISO-8859-1"
    indent="yes"/>

  <!--<xsl:output method="xml"
    indent="yes"
    doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
    doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>-->

<!-- selectionne l'extension lue sur la ligne de commande. Par defaut c'est xml. Comme on veut lire du mathml ne pas mettre html. On obtient directement du mathml content.  A noter que si on veut du mathml presentation il faudra appliquer saxon deux fois (une fois avec focdoc2html.xsl et une autre fois avec mathml.xsl legerement modifiee de D. Carlisle). Donc on ne peut pas mettre alors directement l'extension qu'on souhaite. Cf le Makefile-->
<xsl:param name="ext" select="'.tex'"/>

<!-- extension du document final (defaut: pdf) -->
<xsl:param name="ext-final" select="'.pdf'"/>

<!-- article autonome ou non. Defaut: false -->
<xsl:param name="full-doc" select="'false'"/>

  <xsl:template match="/">
    <xsl:call-template name="enter-doc"/>
    <!--<html xmlns="http://www.w3.org/1999/xhtml">-->
      <xsl:apply-templates select="focdoc"/>
    <xsl:call-template name="end-doc"/>
  </xsl:template>

<xsl:template name="enter-doc">
<xsl:if test="$full-doc='true'">
<!-- preambule et \begin{document} -->
<xsl:text>
\documentclass{article}
\usepackage{ams,focdoc}
\usepackage[latin1]{inputenc}
</xsl:text>
<xsl:call-template name="focdoc-head"/>
<xsl:text>
\begin{document}
\maketitle
</xsl:text>
</xsl:if>
</xsl:template>

<xsl:template name="end-doc">
<xsl:if test="$full-doc='true'">
      <xsl:text>\end{document}</xsl:text>
</xsl:if>
</xsl:template>

<!--Ivan :  commentaires d'especes et de collection + gestions de tags provisoires a supprimer-->
<xsl:template match="provisoire" name="provisoire">
	<xsl:for-each select="text()">
	  <xsl:value-of select="normalize-space()"/>
	</xsl:for-each>
  <xsl:text>

  </xsl:text>
</xsl:template>

 <xsl:template match="focdoc">
  <xsl:apply-templates select="general-informations"/>
  <xsl:call-template name="loads"/>
  <xsl:call-template name="opens"/>
 <!--meriterait d'etre parametre
  <xsl:apply-imports/> -->
      <xsl:for-each select="species|collection">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
 </xsl:template>

  <xsl:template name="focdoc-head">
    <xsl:text>\title{</xsl:text>
    <xsl:value-of select="focdoc/general-informations/title"/>
    <xsl:text>}
      \author{</xsl:text>
    <xsl:value-of select="focdoc/general-informations/author"/>
    <xsl:text>}</xsl:text>
   </xsl:template>

  <xsl:template match="general-informations">
        <xsl:if test="$full-doc='false'">   
          <xsl:choose>   
              <xsl:when test="normalize-space(title)!=''">
                 <xsl:text>\mytitle{</xsl:text>
                 <xsl:value-of select="normalize-space(title)"/>
                 <xsl:text>}</xsl:text>
              </xsl:when>
          </xsl:choose>
          <xsl:choose>
             <xsl:when test="normalize-space(author)!=''">
               <xsl:text>\myauthor{</xsl:text>
               <xsl:value-of select="normalize-space(author)"/>
               <xsl:text>}</xsl:text>
             </xsl:when>
          </xsl:choose>
        </xsl:if>
    <xsl:if test="comments">
     <xsl:text> \myabstract{ </xsl:text>
        <xsl:value-of select="normalize-space(comments)"/>
     <xsl:text>}</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="loads">
    <xsl:if test="load">
     <xsl:text>
      \section*{Files loaded}
        \begin{itemize}
     </xsl:text>
     <xsl:for-each select="load">
        <xsl:text>\item </xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
        <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
        <xsl:text>.foc
        </xsl:text>
     </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xsl:template name="opens">
    <xsl:if test="open">
     <xsl:text>
      \section*{Files opened}
        \begin{itemize}
     </xsl:text>
     <xsl:for-each select="open">
        <xsl:text>\item </xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
        <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
        <xsl:text>.foc
        </xsl:text>
     </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <!-- Print the usual name of the element. The name from the comments or
       the FoC name if the previous one is not given -->
  <xsl:template name="print-usual-name">
    <xsl:choose>
      <xsl:when test="informations/name">
        <xsl:value-of select="normalize-space(informations/name)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="foc-name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Print the name of species or collections with an anchor for
       hyper-referencing -->
  <xsl:template name="print-name">
    <xsl:param name="spec-or-collec"/>
    <xsl:call-template name="print-usual-name"/>
    <xsl:text>\hypertarget{</xsl:text>
        <xsl:value-of select="$spec-or-collec"/>
        <xsl:text>-</xsl:text>
        <xsl:value-of select="normalize-space(foc-name)"/>
    <xsl:text>}{}</xsl:text>
  </xsl:template>


<!-- ancres a modifiees  -->
  <!-- Print the name of species and collection components with an
       anchor for hyper-referencing -->
  <xsl:template name="print-name-component">
    <xsl:call-template name="print-usual-name"/>
    <xsl:text>\hypertarget{</xsl:text>
	<xsl:if test="name(.)='definition'">
		<xsl:text>definition-</xsl:text>
	</xsl:if>
	<xsl:if test="name(.)='property'">
		<xsl:text>property_theorem-</xsl:text>
	</xsl:if>
	<xsl:if test="name(.)='theorem'">
		<xsl:text>property_theorem-</xsl:text>
	</xsl:if>
        <xsl:value-of select="normalize-space(../foc-name)"/>
        <xsl:text>-</xsl:text>
        <xsl:value-of select="normalize-space(foc-name)"/>
      <xsl:text>}{}</xsl:text>
  </xsl:template>


  <xsl:template match="species">
    <xsl:text>\begin{species}{</xsl:text>
      <xsl:call-template name="print-name">
        <xsl:with-param name="spec-or-collec">
          <xsl:text>species</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    <xsl:text>}</xsl:text>
	<!-- ajoute par Ivan, le test sur les infos/comments-->
           <xsl:for-each select="informations/comments">
	             		<xsl:call-template name="provisoire"/>
	       		<xsl:if test="provisoire">
				<xsl:apply-templates select="./provisoire"/>	
			</xsl:if><!--existence de tags provisoires--> 
     	   </xsl:for-each><!--existence de comments-->
 
    <xsl:text>{\tt species </xsl:text>
      <xsl:apply-templates select="foc-name"/>
        <xsl:if test="parameter">
          <xsl:text> (</xsl:text>
          <xsl:for-each select="parameter">
            <xsl:apply-templates select="."/>
            <xsl:if test="not(position()=last())">
                <xsl:text>,</xsl:text>
            </xsl:if>
          </xsl:for-each>
            <xsl:text>)
</xsl:text>
        </xsl:if>
      <xsl:if test="inherits">
        <xsl:text> inherits
  </xsl:text>
        <!-- gestion liens externes au fichier courant  -->
        <xsl:for-each select="inherits">
          <xsl:text>\href{</xsl:text>
          <xsl:if test="foc-name[@infile]">
            <xsl:value-of select="normalize-space(foc-name/@infile)"/>
            <xsl:value-of select="$ext-final"/>
          </xsl:if>				  	
          <xsl:text>#species-</xsl:text>
          <xsl:apply-templates select="foc-name" mode="href"/>
          <xsl:text>}{</xsl:text>
          <xsl:apply-templates select="foc-name"/>
          <xsl:text>}</xsl:text>

          <xsl:if test="not(position()=last())">
            <xsl:text>,</xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>

      <xsl:text>}

      </xsl:text>
      <xsl:if test="carrier">
        <xsl:apply-templates select="carrier"/>
      </xsl:if>
      <xsl:if test="informations/comments/*">
        <xsl:text>\\</xsl:text>
        <xsl:apply-templates select="informations/comments"/>
      </xsl:if>
    <xsl:if test="signature|definition|theorem|property">
      <xsl:text>\begin{body}
      </xsl:text>
        <xsl:for-each select="signature|definition|theorem|property">
          <xsl:apply-templates select="."/>
        <xsl:text>
        </xsl:text>
      </xsl:for-each>
        <xsl:text>\end{body}</xsl:text>
      </xsl:if>
      <xsl:text>\end{species}</xsl:text>
  </xsl:template>

  <xsl:template match="collection">
    <xsl:text>\begin{collection}{</xsl:text>
      <xsl:call-template name="print-name">
        <xsl:with-param name="spec-or-collec">
          <xsl:text>collection</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:text>}</xsl:text>
      <!-- ajoute par Ivan, le test sur les infos/comments-->
      <xsl:for-each select="informations/comments">
        <xsl:call-template name="provisoire"/>
        <xsl:if test="provisoire">
          <xsl:apply-templates select="./provisoire"/>	
        </xsl:if><!--existence de tags provisoires--> 
      </xsl:for-each><!--existence de comments-->
      <xsl:text>{\tt collection </xsl:text>
        <xsl:apply-templates select="foc-name"/>
      <xsl:if test="parameter">
        <xsl:text>(</xsl:text>
        <xsl:for-each select="parameter">
          <xsl:apply-templates select="."/>
          <xsl:if test="not(position()=last())">
            <xsl:text>,</xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>)
</xsl:text>
      </xsl:if>
      <xsl:if test="implements">
        <xsl:text>implements \href{</xsl:text>
        <!-- gestion liens externes au fichier courant  -->
        <xsl:if test="foc-name[@infile]">
          <xsl:value-of select="normalize-space(foc-name/@infile)"/>
          <xsl:value-of select="$ext-final"/>
        </xsl:if>				  	
        <xsl:text>#species-</xsl:text>
        <xsl:apply-templates select="implements/foc-name" mode="href"/>
        <xsl:text>}{</xsl:text>
        <xsl:apply-templates select="implements/foc-name"/>
        <xsl:text>}}

      </xsl:text>
      </xsl:if>

      <xsl:text>

      </xsl:text>
      <xsl:if test="carrier">
        <xsl:apply-templates select="carrier"/>
      </xsl:if>
      <xsl:if test="informations/comments/*">
        <xsl:text>\\</xsl:text>
        <xsl:apply-templates select="informations/comments"/>
      </xsl:if>
      <xsl:if test="signature|definition|theorem|property">
        <xsl:text>\begin{body}</xsl:text>
        <xsl:for-each select="signature|definition|theorem|property">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
        <xsl:text>\end{body}</xsl:text>
      </xsl:if>
      <xsl:text>\end{collection}</xsl:text>
  </xsl:template>

  <xsl:template match="signature">
    <xsl:param name="pere" select="normalize-space(../foc-name)"/>
    <xsl:param name="comefrom" select="normalize-space(history/first)"/>
      <xsl:text>\method{signature}</xsl:text>
      <xsl:call-template name="print-name-component"/>
      <xsl:apply-templates select="type"/>
      <xsl:call-template name="do-history"/>
    <xsl:if test="$comefrom=$pere">
      <!-- cette signature est declare ici pour la premiere fois -->
      <xsl:if test="informations/comments">
        <xsl:text>\\</xsl:text>
        <xsl:apply-templates select="informations/comments"/>
      </xsl:if>
    </xsl:if> <!-- cette est declare ici pour la premiere fois -->
  </xsl:template>

  <xsl:template match="definition">
    <xsl:text>\method{definition}</xsl:text>
    <xsl:call-template name="print-name-component"/>
       <xsl:apply-templates select="type"/>
           <xsl:call-template name="do-history"/>
    <xsl:if test="informations/comments">
      <xsl:text>\\</xsl:text>
      <xsl:apply-templates select="informations/comments"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="theorem|property">
    <xsl:param name="pere" select="normalize-space(../foc-name)"/>
    <xsl:param name="comefrom" select="normalize-space(history/first)"/>
    <!-- <xsl:if test="$comefrom=$pere"> -->
      <!-- ce theorem/property est declare ici pour la premiere fois --> 
      <xsl:choose>
        <xsl:when test="name(.)='theorem'">
          <xsl:text>\method{theorem}</xsl:text>
        </xsl:when>
        <xsl:when test="name(.)='property'">
          <xsl:text>\method{property}</xsl:text>
        </xsl:when>
      </xsl:choose>
      <xsl:call-template name="print-name-component"/>
      <xsl:call-template name="do-history"/>
      <xsl:if test="proposition">
        <xsl:apply-templates select="proposition"/>
      </xsl:if>
        <xsl:text> </xsl:text>
      <xsl:if test="informations/comments">
        <xsl:apply-templates select="informations/comments"/>
      </xsl:if>
    <!-- </xsl:if> 
        ce theorem/property est declare ici pour la premiere fois -->
  </xsl:template>

  <xsl:template match="carrier">
    <xsl:text>carrier type </xsl:text>
    <xsl:call-template name="print-name-component"/>
    <xsl:apply-templates select="type"/>
    <xsl:call-template name="do-history-bis"/>
    <xsl:text> </xsl:text>
    <xsl:if test="informations/comments">
      <xsl:apply-templates select="informations/comments"/>
    </xsl:if>
  </xsl:template>


  <!-- deleguer a l'autre feuille de style-->
  <xsl:template match="proposition">
    <xsl:apply-imports/> 
  </xsl:template>

  <xsl:template match="informations/comments">
    <div class="comments">
      <xsl:value-of select="normalize-space(.)"/>
    </div>
  </xsl:template>

  <xsl:template name="do-history">
    <xsl:if test="not(../foc-name=history/first)">
      <xsl:apply-templates select="history"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="do-history-bis">
    <xsl:if test="not(foc-name=history/first)">
      <xsl:apply-templates select="history"/>
    </xsl:if>
  </xsl:template>

  <!-- gestion des liens externes au fichier -->
  <xsl:template match="history">
      <xsl:text> comes from: \href{</xsl:text>
      <xsl:if test="comes-from[@infile]">
        <xsl:value-of select="normalize-space(comes-from/@infile)"/>
        <xsl:value-of select="$ext-final"/>
      </xsl:if>
      <xsl:text>#species-</xsl:text>
      <xsl:value-of select="normalize-space(comes-from)"/>
      <xsl:text>}{</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
      <xsl:value-of select="normalize-space(comes-from)"/>
        </xsl:with-param>
        </xsl:call-template>
      <xsl:text>} (\href{</xsl:text>
      <xsl:if test="comes-from[@infile]">
        <xsl:value-of select="normalize-space(comes-from/@infile)"/>
        <xsl:value-of select="$ext-final"/>
      </xsl:if>
      <xsl:text>#</xsl:text>
      <xsl:if test="name(..)='definition'">
        <xsl:text>definition-</xsl:text>
      </xsl:if>
      <xsl:if test="name(..)='property'">
        <xsl:text>property_theorem-</xsl:text>
      </xsl:if>
      <xsl:if test="name(..)='theorem'">
        <xsl:text>property_theorem-</xsl:text>
      </xsl:if>
      <xsl:value-of select="normalize-space(comes-from)"/>
      <xsl:text>-</xsl:text>
      <xsl:value-of select="normalize-space(../foc-name)"/>
      <xsl:text>}{method}),
      origin: \href{</xsl:text>
      <xsl:if test="first[@infile]">
        <xsl:value-of select="normalize-space(first/@infile)"/>
        <xsl:value-of select="$ext-final"/>	
      </xsl:if>
      <xsl:text>#species-</xsl:text>
      <xsl:value-of select="normalize-space(first)"/>
      <xsl:text>}{</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
      <xsl:value-of select="normalize-space(first)"/>
        </xsl:with-param>
        </xsl:call-template>
      <xsl:text>} (\href{</xsl:text>
      <xsl:if test="first[@infile]">
        <xsl:value-of select="normalize-space(first/@infile)"/>
        <xsl:value-of select="$ext-final"/>
      </xsl:if>
      <xsl:text>#</xsl:text>
      <xsl:if test="name(..)='definition'">
        <xsl:text>definition-</xsl:text>
      </xsl:if>
      <xsl:if test="name(..)='property'">
        <xsl:text>property_theorem-</xsl:text>
      </xsl:if>
      <xsl:if test="name(..)='theorem'">
        <xsl:text>property_theorem-</xsl:text>
      </xsl:if>
      <xsl:value-of select="normalize-space(first)"/>
      <xsl:text>-</xsl:text>
      <xsl:value-of select="normalize-space(../foc-name)"/>
      <xsl:text>}{method})</xsl:text>
  </xsl:template>


  <xsl:template match="parameter">
    <xsl:call-template name="print-name-component"/>
    <xsl:choose>
      <xsl:when test="@kind='collection'">
        <xsl:text> is </xsl:text>
      </xsl:when>
      <xsl:when test="@kind='entity'">
        <xsl:text> in </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message>
          <xsl:text>wrong value for attribute kind of parameter tag</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="type"/>
  </xsl:template>

  <!-- templates or types -->

  <xsl:template match="type">
    <xsl:choose>
      <!-- Ivan :  normalement ce devrait etre inutile, mais deux precautions valenmt mieux qu'une-->
      <xsl:when test="ancestor::proposition">
        <xsl:text></xsl:text>
      </xsl:when>

      <!--Ivan 21/07-->
      <xsl:when test="name(..)='signature' or name(..)='definition'">
        <xsl:apply-imports/> 
      </xsl:when>
      
      <xsl:otherwise>
        <xsl:apply-templates select="*"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- gestion des liens externes au fichier -->
  <xsl:template match="atom">
    <xsl:choose>
      <xsl:when test="@order='first'">
        <xsl:text> \typename{</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
        <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
        <xsl:text>} </xsl:text>
      </xsl:when>
      <xsl:when test="@order='high'">
        <!-- gestion des liens externes au fichier -->
        <xsl:text>\href{</xsl:text>
        <xsl:choose>
          <xsl:when test="@infile">
            <xsl:value-of select="normalize-space(@infile)"/>
            <xsl:value-of select="$ext-final"/>
            <xsl:text>#species-</xsl:text>
            <xsl:value-of select="normalize-space(.)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>#species-</xsl:text>
            <xsl:value-of select="normalize-space(.)"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>}{\hotypename{</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
        <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
        <xsl:text>}}</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tvar">
    <xsl:text>\typevar{'</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
    <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template match="fct">
    <xsl:variable name="parenthesis">
      <xsl:if test="*[position()=1 and (atom|tvar|self|abst|prop)]">
        <xsl:text>n</xsl:text>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="$parenthesis='y'">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[position()=1]"/>
    <xsl:if test="$parenthesis='y'">
      <xsl:text>)</xsl:text>
    </xsl:if>
    <xsl:text> $\rightarrow$ </xsl:text>
    <xsl:apply-templates select="*[position()=2]"/>  
  </xsl:template>
  
  <xsl:template match="prod">
    <xsl:variable name="parenthesis">
      <xsl:if test="*[position()=1 and (atom|tvar|self|abst|prop)]">
        <xsl:text>n</xsl:text>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="$parenthesis='y'">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[position()=1]"/>
    <xsl:if test="$parenthesis='y'">
      <xsl:text>)</xsl:text>
    </xsl:if>
    <xsl:text> * </xsl:text>
    <xsl:apply-templates select="*[position()=2]"/>  
  </xsl:template>

  <xsl:template match="prop">
    <xsl:text>Prop</xsl:text>
  </xsl:template>

  <xsl:template match="self">
    <xsl:choose>
      <xsl:when test="ancestor::proposition">
        <xsl:text></xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>self</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="abst">
    <xsl:text>abst</xsl:text>
  </xsl:template>

  <xsl:template match="ml">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

  <xsl:template match="ml-c">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

  <xsl:template match="prm">
    <xsl:text>prn[</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>]prn</xsl:text>
  </xsl:template>

  <xsl:template match="app">
    <xsl:text>app[</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>]app</xsl:text>
  </xsl:template>

  <xsl:template match="meth">
    <xsl:apply-templates select="*[.!=foc-name]"/>
    <xsl:text>.</xsl:text>
    <xsl:apply-templates select="foc-name"/>
  </xsl:template>

  <!-- / templates or types -->

  <xsl:template match="foc-name">
    <xsl:text>
        \focname{</xsl:text>
        <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
    <xsl:value-of select="normalize-space(.)"/>
        </xsl:with-param>
        </xsl:call-template>
    <xsl:text>} </xsl:text>
  </xsl:template>

<xsl:template match="foc-name" mode="href">
     <xsl:value-of select="normalize-space(.)"/>
</xsl:template>       

</xsl:stylesheet>
