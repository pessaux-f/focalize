<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0" 
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:mml="http://www.w3.org/1998/Math/MathML"
		xmlns:foc="http://focal.inria.fr/site/index"
		xmlns="http://www.w3.org/1999/xhtml">

  <xsl:import href="proposition.xsl"/>
  <xsl:output method="xml" indent="yes"/>
  <xsl:param name="ext" select="'.xml'"/>
  <xsl:param name="verbose" select="0"/>
  <xsl:param name="docpath" select="''"/>
  <xsl:param name="mmldisplay" select="'yes'"/>



  <xsl:template match="/">
    <xsl:message>  Use the "docpath" variable to specify a path </xsl:message>
    <xsl:call-template name="proc"/>
    <xsl:text disable-output-escaping="yes">
&lt;!DOCTYPE html PUBLIC &quot;-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN&quot;
&quot;http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd&quot;[
&lt;!ENTITY % MATHML.prefixed &quot;INCLUDE&quot; &gt;
&lt;!ENTITY % MATHML.prefix &quot;mml&quot; &gt;
&lt;!ENTITY % SVG.module &quot;IGNORE&quot; &gt;
&lt;!ATTLIST html xmlns:foc CDATA #FIXED &quot;http://focal.inria.fr/site/index&quot; &gt;
]&gt; 
    </xsl:text>
    <html>	
      <xsl:apply-templates select="foc:focdoc"/>
    </html>  
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="proc">
    <!--ajout de la reference a la feuille de style pour mathml-->
    <xsl:processing-instruction name="xml-stylesheet">
      type="text/xsl" href="http://www.orcca.on.ca/MathML/software/mmlctop2_0.xsl"
    </xsl:processing-instruction>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:focdoc">
    <head>
      <xsl:call-template name="focdoc-head"/>
    </head>
    <body>
      <xsl:apply-templates select="foc:general-informations"/>
      <table class="headTA">
	<caption>
	  <ins>
	    <a>
	      <xsl:attribute name="id">
		<xsl:text>head</xsl:text>
	      </xsl:attribute>
	      <xsl:text> </xsl:text>
	    </a>
	  </ins>
	</caption>
	<xsl:call-template name="loads"/>
	<xsl:call-template name="opens"/>
	<xsl:call-template name="coq-requires"/>
	<xsl:call-template name="index"/>
      </table>
      <!--meriterait d'etre parametre-->
      <ins>
	<a>
	  <xsl:attribute name="href">
	    <xsl:value-of select="$docpath"/>
	    <xsl:text>index.html</xsl:text>
	  </xsl:attribute>
	  <xsl:text> back to index of files</xsl:text>
	</a>
      </ins>
      <table class="TB" border="2" WIDTH = "100%" >  
	<xsl:call-template name="top-level"/>
	<xsl:for-each select="foc:species|foc:collection">
	  <xsl:apply-templates select="."/>
	</xsl:for-each>
      </table>
    </body>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="top-level">  
    <xsl:for-each select="foc:theorem|foc:letprop|foc:global-fun
			  |foc:concrete-type">
      <xsl:sort select="foc:foc-name"/>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="focdoc-head">
    <title>
      <xsl:choose>
	<xsl:when test="foc:general-informations/foc:title">
	  <xsl:value-of select="foc:general-informations/foc:title"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="foc:foc-name"/>
	</xsl:otherwise>
      </xsl:choose>
    </title>
    <link rel="stylesheet" href="focdoc.css" type="text/css"/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:general-informations">
    <xsl:choose>
      <xsl:when test="normalize-space (foc:title) != ''">
	<h1>
	  <xsl:value-of select="normalize-space (foc:title)"/>
	</h1>
      </xsl:when>
      <xsl:otherwise>
	<!-- file name!!! -->
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="normalize-space (foc:author) != ''">
	<p class="author">
	  <xsl:value-of select="normalize-space (foc:author)"/>
	</p>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="foc:comments">
      <p class="comments">
	<xsl:value-of select="normalize-space (foc:comments)"/>
      </p>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="loads">
    <xsl:if test="foc:load">
      <tr>
	<td class="loads">
	  <xsl:text>Load </xsl:text>
	</td>
	<td class="loads">
	  <xsl:for-each select="foc:load">
	    <a class="load">
	      <xsl:attribute name="href">
		<xsl:value-of select="$docpath"/>
		<xsl:value-of select="normalize-space (.)"/>
		<xsl:value-of select="$ext"/>
	      </xsl:attribute>
	      <xsl:value-of select="normalize-space (.)"/>
	    </a>
	    <xsl:if test="not (position() = last())">
	      <xsl:text> - </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</td>
      </tr>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="opens">
    <xsl:if test="foc:open">
      <tr>
	<td class="opens">
	  <xsl:text>Open </xsl:text>
	</td>
	<td  class="opens">
	  <xsl:for-each select="foc:open">
	    <ins>		
	      <a class="open">
		<xsl:attribute name="href">
		  <xsl:value-of select="$docpath"/>
		  <xsl:value-of select="normalize-space (.)"/>
		  <xsl:value-of select="$ext"/>
		</xsl:attribute>
		<xsl:value-of select="normalize-space (.)"/>
	      </a>
	    </ins>
	    <xsl:if test="not (position () = last ())">
	      <xsl:text> - </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</td>
      </tr>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="coq-requires">
    <xsl:if test="foc:coq-require">
      <tr>
	<td class="coq-requires">
	  <xsl:text>Coq requires </xsl:text>
	</td>
	<td class="coq-requires">
	  <xsl:for-each select="foc:coq-require">
	    <ins>		
	      <a class="coq-require">
		<xsl:attribute name="href">
		  <xsl:value-of select="$docpath"/>
		  <xsl:value-of select="normalize-space(.)"/>.v
		</xsl:attribute>
		<xsl:value-of select="normalize-space(.)"/>
	      </a>
	    </ins>
	    <xsl:if test="not (position () = last ())">
	      <xsl:text> - </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</td>
      </tr>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="index">
    <xsl:if test="foc:species">
      <tr>
	<td class="index">
	  <xsl:text>List of species </xsl:text>
	</td>
	<td class="index">
	  <xsl:for-each select="foc:species">
	    <ins>
	      <a>
		<xsl:attribute name="href">
		  <xsl:text>#species-</xsl:text>
		  <xsl:apply-templates select="foc:foc-name"/>
		</xsl:attribute>
		<xsl:apply-templates select="foc:foc-name"/>
	      </a>
	    </ins>
	    <xsl:if test="not (position() = last())">
	      <xsl:text> - </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</td>
      </tr>
    </xsl:if>
    <xsl:if test="foc:collection">
      <tr>
	<td class="index">
	  <xsl:text>List of collections </xsl:text>
	</td>
	<td class="index">
	  <xsl:for-each select="foc:collection">
	    <ins>
	      <a>
		<xsl:attribute name="href">
		  <xsl:text>#collection-</xsl:text>
		  <xsl:apply-templates select="foc:foc-name"/>
		</xsl:attribute>
		<xsl:apply-templates select="foc:foc-name"/>
	      </a>
	    </ins>
	    <xsl:if test="not(position()=last())">
	      <xsl:text> - </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</td>
      </tr>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- Print the usual name of the element. The name from the comments or
       the FoC name if the previous one is not given -->
  <xsl:template name="print-usual-name">
    <xsl:choose>
      <xsl:when test="foc:informations/foc:name">
	<xsl:value-of select="normalize-space (foc:informations/foc:name)"/>
      </xsl:when>
      <xsl:otherwise>
	<code>
	  <xsl:value-of select="normalize-space (foc:foc-name)"/>
	</code>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- Print the name of species or collections with an anchor for
       hyper-referencing -->
  <xsl:template name="print-name">
    <xsl:param name="spec-or-collec"/>
    <xsl:call-template name="print-usual-name"/>
    <ins> 
      <a>
	<xsl:attribute name="id">
	  <xsl:value-of select="$spec-or-collec"/>
	  <xsl:text>-</xsl:text>
	  <xsl:value-of select="normalize-space(foc:foc-name)"/>
	</xsl:attribute>
      </a>
    </ins>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ancres a modifiees  -->
  <!-- Print the name of species and collection components with an
       anchor for hyper-referencing -->
  <xsl:template name="print-name-component">
    <xsl:if test="name(.)!='parameter'">
      <xsl:call-template name="print-usual-name"/>
    </xsl:if>
    <ins>
      <a>
	<xsl:attribute name="id">
	  <xsl:if test="name(.) = 'foc:definition'
			or name(.) = 'foc:signature'">
	    <xsl:text>definition_signature-</xsl:text>
	  </xsl:if>
	  <xsl:if test="name(.) = 'foc:property'">
	    <xsl:text>property_theorem-</xsl:text>
	  </xsl:if>
	  <xsl:if test="name(.) = 'foc:theorem'">
	    <xsl:text>property_theorem-</xsl:text>
	  </xsl:if>
	  <xsl:if test="name(.) = 'foc:letprop'">
	    <xsl:text>letprop-</xsl:text>
	  </xsl:if>
	  <xsl:if test="name(.) = 'foc:parameter'">
	    <xsl:text>parameter-</xsl:text>
	  </xsl:if>
	  <xsl:value-of select="normalize-space (../foc:foc-name)"/>
	  <xsl:text>-</xsl:text>
	  <xsl:value-of select="normalize-space (foc:foc-name)"/>
	</xsl:attribute>
	<xsl:text> </xsl:text>
      </a>
    </ins>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:species|foc:collection">
    <xsl:param name="categorie" select="substring-after (name(),'foc:')"/>
    <tr>
      <xsl:comment>
	<xsl:value-of select="$categorie"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="foc:foc-name"/>
      </xsl:comment>
      <td colspan="2"> 
	<h2>
	  <xsl:value-of select="$categorie"/>
	  <xsl:text> </xsl:text>
	  <xsl:call-template name="print-name">
	    <xsl:with-param name="spec-or-collec">
	      <xsl:value-of select="$categorie"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</h2>
      </td>
    </tr>
    <tr class="comments">
      <td colspan="2"> 
	<xsl:if test="foc:informations/foc:comments">
	  <xsl:value-of
	      select="normalize-space(foc:informations/foc:comments)"/>
	</xsl:if>
      </td>
    </tr>       
    <tr>
      <td colspan="2"> 
	<code class="foc">
	  <code class="key1">
	    <xsl:value-of select="$categorie"/>
	  </code>
	  <xsl:text> </xsl:text>
	  <xsl:value-of select="foc:foc-name"/>
	  <xsl:if test="foc:parameter">
	    <xsl:text> </xsl:text>
	    <code class="key2">
	      <xsl:text>(</xsl:text>
	    </code>
	    <xsl:for-each select="foc:parameter">
	      <xsl:apply-templates select="."/>
	      <xsl:if test="not (position() = last())">
		<code class="key2">
		  <xsl:text>,</xsl:text>
		</code>
	      </xsl:if>
	    </xsl:for-each>
	    <code class="key2">
	      <xsl:text>)</xsl:text>
	    </code>
	  </xsl:if>
	  <xsl:choose>
	    <xsl:when test="foc:inherits">
	      <xsl:text> </xsl:text>
	      <code class="key1">
		<xsl:text>inherits</xsl:text>
	      </code>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text> </xsl:text>
	      <code class="key1">
		<xsl:text>implements</xsl:text>
	      </code>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text> </xsl:text>
	  <xsl:apply-templates select="foc:inherits|foc:implements"/>
	</code>
      </td>
    </tr>
    <xsl:if test="foc:carrier">
      <xsl:apply-templates select="foc:carrier"/>
    </xsl:if>
    <xsl:if test="foc:signature|foc:definition|foc:theorem|foc:property
		  |foc:letprop">
      <xsl:for-each select="foc:signature|foc:definition|foc:theorem
			    |foc:property|foc:letprop">
	<xsl:sort select="foc:foc-name"/>
	<xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:if>      
    <div class="refhead">
      <ins>
	<a>
	  <xsl:attribute name="href">
	    <xsl:text>#head</xsl:text>
	  </xsl:attribute>
	  <xsl:text>top</xsl:text>
	</a>
      </ins>
    </div>
  </xsl:template>




  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:definition|foc:signature|foc:theorem|foc:property
		       |foc:carrier|foc:letprop|foc:global-fun
		       |foc:concrete-type">
    
    <xsl:param name="nameSpecies" select="normalize-space(../foc:foc-name)"/>
    <xsl:param name="initial-apparition"
	       select="normalize-space(foc:history/foc:initial-apparition)"/>

    <!-- Debug du test ci-dessous...
	 <xsl:text>nameSpecies: </xsl:text>
	 <xsl:value-of select="$nameSpecies"/><xsl:text>
	 </xsl:text>
	 <xsl:text>initial-apparition: </xsl:text>
	 <xsl:value-of select="$initial-apparition"/><xsl:text>
	 </xsl:text>
    -->

    <!-- ne pas tout deplier si verbose = 0, tout afficher si =1-->
    <xsl:if test="(($verbose = 0) and ($initial-apparition=$nameSpecies)) or
		  $verbose >= 1 or name(..) = 'foc:collection'
		  or name(..) = 'foc:focdoc'">
      <tr>
	<!--<td class="categ" width="10%">-->
	<td colspan="2">
	  <div class="categ" >
	    <xsl:value-of select="substring-after(name(),'foc:')"/>
	    <xsl:if test="normalize-space(@recursive) = 'yes'"> 
	      <xsl:text> recursive </xsl:text>
	    </xsl:if>
	    <xsl:text> : </xsl:text>
	  </div>
	  <div class="name">
	    <xsl:call-template name="print-name-component"/>
	  </div>
	</td>
      </tr>
      <tr>
	<td colspan="2"> 
	  <code>
	    <xsl:choose>
	      <xsl:when test="name()='foc:concrete-type'">
		<xsl:call-template name="concrete-type"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:if test="name() = 'foc:letprop'">
		  <xsl:call-template name="letprop-parameters"/>
		</xsl:if>
		<xsl:apply-templates select="foc:type|foc:proposition"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </code>
	</td>
      </tr>
      <xsl:call-template name="do-history-comments"/>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="letprop-parameters">
    <xsl:text>letprop parameters : </xsl:text>
    <xsl:for-each select="foc:param-prop">
      <xsl:value-of select="normalize-space (foc:foc-name)"/>
      <xsl:text> of type </xsl:text>
      <xsl:apply-templates select="foc:type"/>
      <xsl:if test="not (position() = last())">            
	<xsl:text> , </xsl:text>        
      </xsl:if>
    </xsl:for-each>
    <br/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="concrete-type">
    <xsl:text>parameters of type: </xsl:text>
    <xsl:for-each select="foc:ty-param">
      <xsl:value-of select="normalize-space (.)"/>
      <xsl:text> : Type </xsl:text>
      <xsl:if test="not (position() = last())">            
	<xsl:text> , </xsl:text>        
      </xsl:if>
    </xsl:for-each>
    <xsl:if test="foc:constr">
      <br/>
      <xsl:text>constructors :  </xsl:text>
      <br/>
      <xsl:for-each select="foc:constr">
	<xsl:apply-templates select="foc:type"/>    
	<xsl:if test="not (position() = last())">            
	  <br/>       
	</xsl:if>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- deleguer a l'autre feuille de style-->
  <xsl:template match="foc:proposition">
    <xsl:choose>
      <xsl:when test="normalize-space($mmldisplay)='yes'">
	<code class="proposition">  
	  <xsl:apply-imports/>  
	</code>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>MathML Display not selected</xsl:text>
      </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:informations/foc:comments">
    <div class="comments">
      <xsl:copy-of select="text()|*"/>
    </div>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template name="do-history-comments">
    <!-- A priori le noeud courant est  un fils de species ou collection-->
    <xsl:if test="not(normalize-space(../foc:foc-name)=
		  normalize-space(foc:history/foc:initial-apparition))
		  or foc:informations/foc:comments or foc:fun-dep">
      <!--<td colspan="2"/>-->
      <xsl:if test="foc:informations/foc:comments">
	<tr>
	  <td colspan="2">
	    <xsl:apply-templates select="foc:informations/foc:comments"/>
	  </td>
	</tr>
      </xsl:if>   
            
      <xsl:if test="not (normalize-space (../foc:foc-name) =
		    normalize-space (foc:history/foc:initial-apparition))
		    or foc:fun-dep">
	<tr>
	  <xsl:choose>
	    <!--attention : pour le moment les global-fun n'ont pas de fils
		fun-dep mais ca peut changer-->
	    <xsl:when test="not (normalize-space(../foc:foc-name) =
			    normalize-space(foc:history/foc:initial-apparition))
			    and not (foc:fun-dep) 
			    and not (name(.) = 'foc:global-fun'
			    or name(.) = 'foc:concrete-type')">
	      <td>              
		<xsl:apply-templates select="foc:history"/>
	      </td>
	      <td/>
	    </xsl:when>
	    <xsl:when test="not (normalize-space(../foc:foc-name) =
			    normalize-space(foc:history/foc:initial-apparition))
			    and foc:fun-dep">
	      <td>
		<xsl:apply-templates select="foc:history"/>
	      </td>    
	      <td>
		<xsl:apply-templates select="foc:fun-dep"/>
	      </td>                        
	    </xsl:when>
	    <xsl:when test="foc:fun-dep">
	      <td>
		<xsl:apply-templates select="foc:fun-dep"/>
	      </td>
	      <td/>
	    </xsl:when>
	  </xsl:choose>
	</tr>
      </xsl:if>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:history">
    <div class="history">
      <xsl:apply-templates select="foc:comes-from|foc:initial-apparition"/>
    </div>   
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:fun-dep">
    <xsl:text>Mutually recursives functions:  </xsl:text>
    <xsl:for-each select="./foc:foc-name">
      <xsl:apply-templates select="."/>
      <xsl:if test="position() != last()">
	<xsl:text> , </xsl:text> 
      </xsl:if>
    </xsl:for-each>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:comes-from|foc:initial-apparition">
    <xsl:value-of select="substring-after(name(),'foc:')"/>
    <xsl:text> : </xsl:text>
    <ins> 
      <a>
	<xsl:attribute name="href">
	  <xsl:value-of select="$docpath"/>
	  <xsl:if test="@infile">
	    <xsl:value-of select="normalize-space (@infile)"/>
	    <xsl:value-of select="$ext"/>
	  </xsl:if>
	  <xsl:text>#species-</xsl:text>
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:attribute>
	<xsl:value-of select="normalize-space(.)"/>
      </a>
    </ins>
    <xsl:text>(</xsl:text>

    <ins>
      <a>
	<xsl:attribute name="href">
	  <xsl:value-of select="$docpath"/>	  
	  <xsl:if test="@infile">
	    <xsl:value-of select="normalize-space (@infile)"/>
	    <xsl:value-of select="$ext"/>
	  </xsl:if>
	  <xsl:text>#</xsl:text>
	  <xsl:if test="ancestor::foc:definition|ancestor::foc:signature">
	    <xsl:text>definition_signature-</xsl:text>
	  </xsl:if>
	  <xsl:if test="ancestor::foc:property|ancestor::foc:theorem">
	    <xsl:text>property_theorem-</xsl:text>
	  </xsl:if>
	  <xsl:value-of select="normalize-space (.)"/>
	  <xsl:text>-</xsl:text>
	  <xsl:value-of select="normalize-space (../../foc:foc-name)"/>      
	</xsl:attribute>
	<xsl:value-of select="normalize-space (../../foc:foc-name)"/>
      </a>
    </ins>
    <xsl:text>)</xsl:text>
    <xsl:if test="following-sibling::*">
      <br/>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:parameter">
    <xsl:call-template name="print-name-component"/>
    <xsl:choose>
      <xsl:when test="normalize-space(@kind) = 'collection'">
	<xsl:text> : </xsl:text>
	<xsl:apply-templates select="foc:type/foc:atom" mode="high"/>
      </xsl:when>
      <xsl:otherwise>
	<mml:math>
	  <mml:apply>
	    <mml:in/>
	    <mml:ci></mml:ci>
	    <mml:ci></mml:ci>
	  </mml:apply>
	</mml:math>
	<xsl:apply-templates select="foc:type"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> </xsl:text>
    <code class="key2">
      <xsl:choose>
	<xsl:when test="@kind != 'entity' and @kind != 'collection' ">
	  <xsl:message>
	    <xsl:text>wrong value for attribute kind of parameter tag</xsl:text>
	  </xsl:message>
	</xsl:when>
      </xsl:choose>
    </code>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- templates or types -->
  <xsl:template match="foc:type">
    <xsl:choose>
      <xsl:when test="normalize-space($mmldisplay)='yes'">		
	<xsl:apply-imports/> 
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>MathML Display not selected</xsl:text>
      </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:atom" mode="high">
    <!-- ne traite pas le cas des parametres qui sont des especes
	 paramaetrees -->
    <!--<xsl:choose>
	<xsl:when test="@order='first'">
	<xsl:value-of select="normalize-space(.)"/>
	</xsl:when>
	<xsl:when test="@order='high'">-->
    <ins>
      <a>
	<xsl:attribute name="href">
	  <xsl:value-of select="$docpath"/>
	  <xsl:choose>
	    <xsl:when test="@infile">
	      <xsl:value-of select="normalize-space (@infile)"/>
	      <xsl:value-of select="$ext"/>
	      <xsl:text>#species-</xsl:text>
	      <xsl:value-of select="normalize-space (.)"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>#species-</xsl:text>
	      <xsl:value-of select="normalize-space (.)"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
	<xsl:value-of select="normalize-space (.)"/>
      </a>
    </ins>  
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:tvar">
    <xsl:value-of select="normalize-space (.)"/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:fct">
    <xsl:variable name="parenthesis">
      <xsl:if test="*[position() = 1 and
		    (foc:atom|foc:tvar|foc:self|foc:abst|foc:prop)]">
	<xsl:text>n</xsl:text>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="$parenthesis='y'">
      <code class="key2">
	<xsl:text>(</xsl:text>
      </code>
    </xsl:if>
    <xsl:apply-templates select="*[position() = 1]"/>
    <xsl:if test="$parenthesis='y'">
      <code class="key2">
	<xsl:text>)</xsl:text>
      </code>
    </xsl:if>
    <code class="key2">
      <xsl:text> -> </xsl:text>
    </code>
    <xsl:apply-templates select="*[position() = 2]"/>  
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:prod">
    <xsl:variable name="parenthesis">
      <xsl:if test="*[position() = 1
		    and (foc:atom|foc:tvar|foc:self|foc:abst|foc:prop)]">
	<xsl:text>n</xsl:text>
      </xsl:if>
    </xsl:variable>
    <xsl:if test="$parenthesis = 'y'">
      <code class="key2">
	<xsl:text>(</xsl:text>
      </code>
    </xsl:if>
    <xsl:apply-templates select="*[position()=1]"/>
    <xsl:if test="$parenthesis = 'y'">
      <code class="key2">
	<xsl:text>)</xsl:text>
      </code>
    </xsl:if>
    <code class="key2">
      <xsl:text> * </xsl:text>
    </code>
    <xsl:apply-templates select="*[position() = 2]"/>  
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:prop">
    <code class="key3">
      <xsl:text>Prop</xsl:text>
    </code>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:self">
    <xsl:choose>
      <xsl:when test="ancestor::foc:proposition">
	<xsl:text></xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<code class="key3">
	  <xsl:text>Self</xsl:text>
	</code> 
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:abst">
    <code class="key3">
      <xsl:text>abst</xsl:text>
    </code>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:ml">
    <code class="caml">
      <xsl:value-of select="normalize-space (.)"/>
    </code>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:ml-c">
    <code class="caml">
      <xsl:value-of select="normalize-space(.)"/>
    </code>
  </xsl:template>


  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:prm">
    <xsl:text>prn[</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>]prn</xsl:text>
  </xsl:template>
  <!--
      <xsl:template match="foc:meth">
      <xsl:apply-templates select="*[.!=foc:foc-name]"/>
      <code class="key2">
      <xsl:text>.</xsl:text>
      </code>
      <xsl:apply-templates select="foc:foc-name"/>
      </xsl:template>
  -->


  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:inherits|foc:implements">
    <ins>
      <a>
	<xsl:attribute name="href">
	  <xsl:value-of select="$docpath"/>
	  <xsl:choose>    
	    <xsl:when test="foc:app/foc:foc-name[@infile]">
	      <xsl:value-of
		  select="normalize-space (foc:app/foc:foc-name/@infile)"/>
	      <xsl:value-of select="$ext"/>
	    </xsl:when>
	    <xsl:when test="foc:atom[@infile]">
	      <xsl:value-of select="normalize-space (foc:atom/@infile)"/>
	      <xsl:value-of select="$ext"/>
	    </xsl:when>                
	  </xsl:choose> 	  	
	  <xsl:text>#species-</xsl:text>
	  <xsl:apply-templates select="foc:atom|foc:app" mode="make-href"/>
	</xsl:attribute>
	<xsl:apply-templates select="foc:atom|foc:app" mode="display-of-link"/>
      </a>
    </ins>
    <xsl:if test="following-sibling::*[position() = 1
		  and name(.) = 'foc:inherits']">
      <code class="key2">
	<xsl:text>,</xsl:text>
      </code>
    </xsl:if>
    <xsl:if test="following-sibling::*[position() = 1
		                       and name(.) = 'foc:implements']">
      <code class="key2">
	<xsl:text>,</xsl:text>
      </code>
    </xsl:if>         
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:app" mode="display-of-link">
    <xsl:value-of select="normalize-space(foc:foc-name)"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[position() > 1]" mode="display-of-link"/>
    <xsl:text>)</xsl:text>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:param" mode="display-of-link">
    <xsl:value-of select="normalize-space (.)"/>
    <xsl:if test="following-sibling::foc:param">
      <xsl:text>,</xsl:text>
    </xsl:if>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:atom" mode="display-of-link">
    <xsl:value-of select="normalize-space ()"/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:app"  mode="make-href">
    <xsl:value-of select="normalize-space (foc:foc-name)"/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:atom" mode="make-href">
    <xsl:value-of select="normalize-space ()"/>
  </xsl:template>



  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <!-- ********************************************************************* -->
  <xsl:template match="foc:foc-name">
    <xsl:value-of select="normalize-space (.)"/>
  </xsl:template>

</xsl:stylesheet>
