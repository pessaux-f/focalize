<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:strip-space elements="nb_total count" />

  <!-- <xsl:output indent="yes"/> -->
  <!-- The body of the document -->
	<xsl:template match="/">
		<html>
		<head>
			<title>Rapport de Test</title>
			<link rel="stylesheet" href="test_report.css" type="text/css" />
		</head>
		<body>			
      		<xsl:apply-templates />			 
		</body>
		</html>
	</xsl:template>
  <!-- ***************************** -->

  <!--  Species under test -->
  <xsl:template match="context_test">
    <table class="cssspeciestable">
      <xsl:apply-templates select="collections/collection" />
      <tr>
        <td class="cssspecies">
          <xsl:apply-templates select="species" />
        </td>
      </tr>
    </table>
	</xsl:template>

  <xsl:template match="collection">
    <tr>
      <td class="csscollectionname">
        let <xsl:value-of select="paramname" /> =
        <xsl:apply-templates select="species" /> in <br />
      </td>
    </tr>
	</xsl:template>

  <xsl:template match="species">
    <xsl:value-of select="module" /> #
    <xsl:value-of select="name" />
    <xsl:if test="count(specparameters) &gt; 0">
      (<xsl:for-each select="specparameters/*">
        <xsl:if test="position() = 1"><xsl:value-of select="." /> </xsl:if>
        <xsl:if test="position() != 1">,<xsl:value-of select="." /> </xsl:if> 
      </xsl:for-each>
      )
    </xsl:if>
  </xsl:template>

  <!--  The kind of parameters -->
  <xsl:template match="paramcoll">
    <xsl:value-of select="pcollname" /> is <xsl:value-of select="pcollvalue" />
  </xsl:template>

  <xsl:template match="parament">
    <xsl:value-of select="pentname" /> = <xsl:apply-templates select="pentvalue" />
	</xsl:template>

  <!-- **************************** -->
  <!-- ***** The property name (printed out only if there is more than one
             elementary form) *** -->

	<xsl:template match="propriete">
		<div class="csspropriete">
			<h1 class="cssname"><xsl:value-of select="name" /></h1>		
      <xsl:if test='count(elementaire) != 1'>
        <h2 class="cssforme"><xsl:value-of select="forme" /></h2>		
      </xsl:if>

      <xsl:apply-templates select="elementaire" />
    </div>
  </xsl:template>


  <!-- **************************** -->
  <!-- ***** return 1 of all elements are ok 0 otherwise *** -->

  <!-- **************************** -->
  <!-- ***** The elementary form  *** -->
	<xsl:template match="elementaire">
    		
    <xsl:variable name="vars"> 
      <xsl:apply-templates select="forme/variables" />
    </xsl:variable>

    <!-- take the list of conclusion for later -->
    <xsl:variable name="conclusions" select="forme/conclusions/focexpr" />
    <xsl:variable name="preconditions" select="forme/preconditions/focexpr" />

    <!-- We have to print the result of the precondition if at least 1 has been
    evaluated to false --> 
    <xsl:variable name="printprecond">
      <xsl:choose>
        <xsl:when test="count(test/preconds/precond/ok) &lt; count(test/preconds/precond)">
          1
        </xsl:when>
        <xsl:otherwise>
          0
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

            <!-- Output the elementary form -->
		<div class="csselementaire">
      <h3>
        <table class="cssformeelementaire">
        <tr>
          <td> 
            <!-- the foralls  -->
            <div class="csselemforall">

              <xsl:if test="count(forme/variables/variable) &gt; 0">
                <xsl:for-each select="forme/variables/variable">
                  <xsl:if test="position() = 1">&#8704;(<xsl:value-of select="varname" /> </xsl:if>
                  <xsl:if test="position() != 1">
                    <xsl:variable name="pred" select="preceding-sibling::variable[position()=1]"/>
                    <xsl:if test="$pred/vartype != ./vartype"> : <xsl:apply-templates select="$pred/vartype" />) (</xsl:if>
                    <xsl:if test="$pred/vartype = ./vartype"> <!-- &#160; -->, </xsl:if> <xsl:value-of select="varname" />
                  </xsl:if>
                </xsl:for-each>
              <xsl:variable name="pred" select="forme/variables/variable[position()=last()]"/> : <xsl:apply-templates select="$pred/vartype" />), </xsl:if> </div> </td> 
          <td> 
            <!-- precondition -->
            <div class="csselemprecondition">
              <xsl:apply-templates select="forme/preconditions" />
            </div> 
          </td> 
          <td> 
            <xsl:if test="count(forme/preconditions/focexpr) &gt; 0">
              <div class="csselementail">
                &#8658; <!-- &#8660; &#8596; &#8594; -->
              </div>
            </xsl:if>
          </td> 
          <td> 
                   <!-- conclusion -->
            <div class="csselemconclusion">
              <xsl:apply-templates select="forme/conclusions" /> 
            </div> 

          </td>
        </tr>
        </table>
      </h3>

            <!-- ************************** -->
            <!-- Output the name and value of each precondition (P1 = ...) and conclusion (C1 = ...) -->
      <table class="csstestlegend">
        <tr> <td>
          <xsl:if test="$printprecond = 1">
          <xsl:for-each select="$preconditions">
            P<xsl:number value="position()" format="1" /> = <xsl:apply-templates select="." />  <br/>
          </xsl:for-each>
          </xsl:if>
          <br />
          <xsl:if test="count($conclusions) &gt; 1">
            <xsl:for-each select="$conclusions">
              C<xsl:number value="position()" format="1" /> = <xsl:apply-templates select="." />  <br/>
            </xsl:for-each>
          </xsl:if>
      </td> </tr>
      </table>
            <!-- Output the set of test case -->
      <table class="csstests">
          <xsl:if test="count(test) &gt; 0">
            <tr>
              <!-- The header line -->
              <th class="csstitreposition">#</th>					
              <xsl:copy-of select="$vars" /> <!-- var name -->
              <!-- Conclusion name -->

              <xsl:if test="$printprecond = 1">
                <xsl:for-each select="$preconditions">
                  <th class="csstitreprecond">P<xsl:number value="position()" format="1" /></th>
                </xsl:for-each>
                <td class="csspreconsep" />
              </xsl:if>

              <xsl:if test="count($conclusions) &gt; 1">
                <xsl:for-each select="$conclusions">
                  <th class="csstitreconclu">C<xsl:number value="position()" format="1" /></th>
                </xsl:for-each>
              </xsl:if>

              <xsl:apply-templates select="conclusions/focexpr" />
              <th class="csstitreresultat">Résultat</th>
            </tr>
            <!-- The lines of the table -->
            <xsl:for-each select="test">
              <tr>
                <td class="cssnum"><xsl:number value="position()" format="1"/></td>
                <xsl:apply-templates select="value" />
                <xsl:if test="$printprecond = 1">
                  <xsl:apply-templates select="preconds/*" />
                  <td class="csspreconsep" />
                </xsl:if>
                <xsl:if test="count($conclusions) &gt; 1">
                  <xsl:apply-templates select="conclus/*" />
                </xsl:if>
                <xsl:apply-templates select="resultat" />
              </tr>
            </xsl:for-each>
          </xsl:if>
          <xsl:if test="count(test) = 0">
            <tr>No test case found</tr>
          </xsl:if>
          <!--  -->
      </table>
    </div>
    <div><br/></div>
    <div class="cssstats">
      Couverture de la conclusion : <xsl:value-of xml:space="default" select="stats/conclucoverage/nb_reach" /> / <xsl:value-of select="stats/conclucoverage/nb_total" /><br/>
      Nombre de jeux de test KO : <xsl:value-of select="count(test/resultat/ko)" /><br/>
      Nombre de jeux de test generés : <xsl:value-of select="stats/nb_generated" /><br/>
      Temps de génération : <xsl:value-of select="stats/times" /> ms <br/>
      Methode de génération: <xsl:value-of select="stats/method" />
    </div> 


  </xsl:template>

  <!--  Show the precondition as a conjunction -->
  <xsl:template match="preconditions"> <!-- not -> &#172  &not; -->
    <xsl:if test="count(focexpr) &gt; 1">
      <xsl:for-each select="focexpr">
        <xsl:choose>
          <xsl:when test="position() != 1">
            <br/> <!-- and --> &#8743; <xsl:apply-templates select="." />
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="." />
          </xsl:otherwise>
        </xsl:choose> 
      </xsl:for-each>
    </xsl:if>
    <xsl:if test="count(focexpr) = 1">
      <xsl:apply-templates select="focexpr" />
    </xsl:if>
      <!--   </th> -->
	</xsl:template>
  <!-- ************************* -->

  <!--  Show the conclusion as a disjunction -->
    <xsl:template match="conclusions">
      <xsl:for-each select="focexpr">
        <xsl:choose>
          <xsl:when test="position() != last()">
            <xsl:apply-templates select="." /> &#8744; <!-- or --><br/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="." />
          </xsl:otherwise>
        </xsl:choose> 
      </xsl:for-each>
	</xsl:template>
  <!-- ************************* -->


  <!-- The variable names in the test cases tables -->
	<xsl:template match="variable">
		<th class="csstitrevalue">
			<xsl:value-of select="varname" />
		</th>
	</xsl:template>
  <!-- *************************** -->
  <!-- The conclusions statement in the test cases tables -->
  <xsl:template match="conclusion">
    <th class="csstitreconclusion">
			<xsl:value-of select="." />
		</th>
	</xsl:template>
  <!-- *************************** -->

  <!-- The variables values -->
	<xsl:template match="value">
    <td class="cssvalue">
      <!-- <xsl:value-of select="valstring" /> -->
      <xsl:apply-templates select="valexpr" />
    </td>
	</xsl:template>
  <!-- ******************** -->
	
	<xsl:template match="badprecond">
    <td>précondition non vérifiée</td>
	</xsl:template>

	<xsl:template match="resultat">
				<xsl:apply-templates select="ok" />
				<xsl:apply-templates select="ko" />
				<xsl:apply-templates select="raise" />
	</xsl:template>

  <xsl:template match="conclu">
				<xsl:apply-templates select="ok" />
				<xsl:apply-templates select="ko" />
				<xsl:apply-templates select="raise" />
	</xsl:template>

  <!-- the verdicts -->  
  <xsl:template match="ok">
    <td class="cssresultatOK">OK</td>
	</xsl:template>

  <xsl:template match="ko">
    <td class="cssresultatKO">KO</td>
	</xsl:template>

  <xsl:template match="raise">
    <td class="cssresultatKO">Exception : <xsl:value-of select="." /></td>
	</xsl:template>
  <!-- *************** -->	

  <xsl:template match="vartype">
				<xsl:apply-templates select="typeatom" />
				<xsl:apply-templates select="typeprmatom" />
				<xsl:apply-templates select="typefct" />
				<xsl:apply-templates select="typeprod" />
				<xsl:apply-templates select="typeprm" />
	</xsl:template>
  
  <xsl:template match="typeprm">
		<xsl:apply-templates select="prmname" />(<xsl:apply-templates select="prmlist" />)</xsl:template>

  <xsl:template match="typeatom">
     <xsl:value-of select="." />
	</xsl:template>

  <xsl:template match="typeprmatom">
     <xsl:value-of select="." />
  </xsl:template>

  <xsl:template match="typefct">
    <!--
    <xsl:if test="count(forme/preconditions/precondition) &gt; 0">
      Utiliser les priorité des operateurs pour mettre le minimum de parentheses
    </xsl:if>
    -->
    (
    <xsl:apply-templates select="left" /> <!-- -&gt; --> &#8594; 
    <xsl:apply-templates select="right" />
    )
  </xsl:template>

  <xsl:template match="typeprod">
    (
    <xsl:apply-templates select="left" /> *
    <xsl:apply-templates select="right" />
     )
  </xsl:template>

  <xsl:template match="typprm">
    TODO
  </xsl:template>

  <xsl:template match="focexpr">
				<xsl:apply-templates select="exprif" />
				<xsl:apply-templates select="exprapp" />
				<xsl:apply-templates select="exprfun" />
				<xsl:apply-templates select="exprvar" />
				<xsl:apply-templates select="exprint" />
				<xsl:apply-templates select="exprstring" />
				<xsl:apply-templates select="exprcaml_def" />
				<xsl:apply-templates select="exprmeth" />
				<xsl:apply-templates select="exprmatch" />
				<xsl:apply-templates select="exprlet" />
				<xsl:apply-templates select="exprglobid" />
	</xsl:template>
  
  <xsl:template match="exprif">
    if <xsl:apply-templates select="cond" /> then <xsl:apply-templates select="then" /> else <xsl:apply-templates select="else" />
	</xsl:template>

  <xsl:template match="exprapp">

    <xsl:choose>
      <!-- If we apply an infix operator -->
      <xsl:when test="(count(appright) = 2) and (count(appleft/exprglobid/infix) = 1)">
        <xsl:apply-templates select="appright[1]" />
        <!-- <xsl:text>&#xa0;</xsl:text> -->
        <xsl:value-of select="appleft/exprglobid/infix" />
        <!-- <xsl:text>&#xa0;</xsl:text> -->
        <xsl:apply-templates select="appright[2]" />
        
      </xsl:when>
      <!--            Otherwise         -->
      <xsl:otherwise>
        <xsl:apply-templates select="appleft" />(<xsl:for-each select="appright"><xsl:choose><xsl:when test="position() != 1">, <xsl:apply-templates select="." /></xsl:when><xsl:otherwise><xsl:apply-templates select="." /></xsl:otherwise></xsl:choose></xsl:for-each>)
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template match="exprfun">
    fun (<xsl:apply-templates select="funvar" /> : <xsl:apply-templates select="funtype" />) &#8594; <xsl:apply-templates select="funexpr" />
  </xsl:template>

  <xsl:template match="exprvar">
     <xsl:value-of select="varname" />
  </xsl:template>

  <xsl:template match="exprint">
     <xsl:value-of select="." />
  </xsl:template>

  <xsl:template match="exprstring">
     "<xsl:value-of select="." />"
  </xsl:template>

  <xsl:template match="exprcaml_def">
     caml <xsl:value-of select="." />
  </xsl:template>

  <xsl:template match="exprmeth">
    <xsl:apply-templates select="collname" />!<xsl:apply-templates select="methname" />
  </xsl:template>

  <xsl:template match="exprmatch">
    match <xsl:apply-templates select="matchval" /> with
  </xsl:template>

  <xsl:template match="exprpattern">
    | <xsl:value-of select="patroot" /> (
    <xsl:for-each select="patappl">
      <xsl:choose>
        <xsl:when test="position() != 1">
          , <xsl:apply-templates select="." />
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="." />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    ) &#8594;  <xsl:apply-templates select="patexpr" />
  </xsl:template>

  <xsl:template match="exprlet">let <xsl:value-of select="letvar" /> = <xsl:apply-templates select="letvarexpr" /> in <xsl:apply-templates select="letexpr" /></xsl:template>

  <xsl:template match="exprglobid">
				<xsl:apply-templates select="prefix" />
				<xsl:apply-templates select="infix" />
  </xsl:template>

  <xsl:template match="infix">
        (<xsl:value-of select="." />)
  </xsl:template>

  <xsl:template match="prefix">
        <xsl:value-of select="module" />#<xsl:value-of select="name" /> 
  </xsl:template>

</xsl:stylesheet>
