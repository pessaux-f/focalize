<?xml version="1.0" encoding="utf-8"?>
<!-- FoCDoc to HTML (MathML). Taggs proposition and descendants -->

<xsl:stylesheet 
  xmlns:mml="http://www.w3.org/1998/Math/MathML"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:foc="http://focal.inria.fr/site/index"
  version="1.0">
 
 <!-- juste pour tests <xsl:param name="docpath" select="''"/>-->

<xsl:template match="int">
<mml:mn><xsl:value-of select="@val"/></mml:mn>
</xsl:template>

<xsl:template match="string">
<mml:ms><xsl:value-of select="@val"/></mml:ms>
</xsl:template>

<!--  utile pour les tests :-->
 <xsl:template match="/">
  <root>
 <xsl:apply-templates select="//foc:proposition"/>
 </root>
 </xsl:template>

<xsl:template match="foc:focdoc">
<xsl:text>

</xsl:text>

</xsl:template>

<xsl:template match="foc:proposition">
<mml:math>
<!--<mml:mstyle color="teal">-->
    <xsl:apply-templates/>
</mml:math>

</xsl:template>


 
  <xsl:template match="foc:all">
  <xsl:element name="mml:apply">
  <mml:forall/>
    <xsl:apply-templates/>
  </xsl:element>
 </xsl:template>

 <xsl:template match="foc:ex">
 <xsl:element name="mml:apply">
 <mml:exists/>
     <xsl:apply-templates/>
 </xsl:element>
 </xsl:template>

 <xsl:template match="foc:var">
  <xsl:param name="nom">
 <xsl:value-of select="normalize-space(foc:foc-name)"/>
 </xsl:param>
<xsl:choose>
 <xsl:when test="name(..)='foc:all' or name(..)='foc:ex'">
   <mml:bvar>
<!--<mml:ci>
  <xsl:value-of select="normalize-space(foc:foc-name)"/>
 </mml:ci>-->
  <xsl:apply-templates select="following-sibling::foc:type[position()=1]" mode="brother_of_var">
 <xsl:with-param name="nom" select="$nom"/>
 </xsl:apply-templates>
 </mml:bvar>
   </xsl:when>
 <xsl:otherwise>
  <mml:ci>
  <xsl:value-of select="normalize-space(foc:foc-name)"/>
  </mml:ci>  
 </xsl:otherwise>
 </xsl:choose>
 </xsl:template>

<!-- 
     tries to find a mathml symbol for the meth-name method of spec
     species by inspecting its ancestor. If the method appears for the
     first time in spec, then its plain name is output
 -->
<xsl:template name="find-symbol">
  <xsl:param name="meth-name"/>
  <xsl:param name="spec"/>
  <xsl:param name="file"/>
  <xsl:param name="proposition-name"/>
  <!--seulement pour les tests ce $proposition-name -->
  <xsl:variable name="my-proposition-name">
        <xsl:value-of select="$proposition-name"/>
  </xsl:variable>

<!-- pere du node de foc-name=$meth-name -->
  <xsl:variable name="meth-node"
    select="$spec/*/foc:foc-name[normalize-space()=$meth-name]/.."/>
  <xsl:variable name="self" select="normalize-space($spec/foc:foc-name)"/>
  <xsl:variable name="parent-name" 
    select="normalize-space($meth-node/foc:dependence/foc:comes-from)"/>
  <!-- useful debugging informations. uncomment if something goes
     wrong 
  <xsl:message>
    <xsl:text>Inspecting: </xsl:text>
    <xsl:value-of select="$meth-name"/>
    <xsl:text> 
        from species </xsl:text>
    <xsl:value-of select="$self"/>
    <xsl:text> 
        parent: </xsl:text>
    <xsl:value-of select="$parent-name"/>
     <xsl:text> 
       meth-name: </xsl:text>
    <xsl:value-of select="$meth-name"/>
    <xsl:text>  
       meth-node/foc-name: </xsl:text>
    <xsl:value-of select="$meth-node/foc:foc-name"/>
     <xsl:text> 
       self: </xsl:text>
    <xsl:value-of select="$self"/>
    <xsl:text> file: </xsl:text>
    <xsl:value-of select="$file"/>
    <xsl:text> 
       proposition (if any): </xsl:text>
    <xsl:value-of select="$proposition-name"/>
  </xsl:message>-->
  <!--<xsl:if test="not($meth-node)">
    <xsl:message>
      Warning: no node found 
    </xsl:message>
  </xsl:if>-->  
 
  <xsl:choose>
    <xsl:when test="$meth-node/foc:informations/foc:math">
      <xsl:apply-templates select="$meth-node/foc:informations/foc:math"/>
    </xsl:when>
    <xsl:when test='not(boolean($parent-name)) or $self=$parent-name'>
      <mml:ci><xsl:value-of select="$meth-name"/></mml:ci>
    </xsl:when>
    <xsl:when test="$meth-node/foc:dependence/foc:comes-from/@infile">
      <!-- we must load another document... -->

      <xsl:variable name="myfile">
        <xsl:value-of select="concat($docpath,$meth-node/foc:dependence/foc:comes-from/@infile)"/>
        <xsl:text>.focdoc</xsl:text>
        <!--
             NdV: should parameterize the suffix + potentially search
             in other directories.
             -->
      </xsl:variable>
      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" select="$meth-name"/>
        <xsl:with-param name="spec"
          select="document(string($myfile))/foc:focdoc/*/foc:foc-name[normalize-space()=$parent-name]/.."/>
        <xsl:with-param name="file" select="$myfile"/>
        <xsl:with-param name="proposition-name" select="$my-proposition-name"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$file">
      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" select="$meth-name"/>
        <xsl:with-param name="spec" select="document(string($file))/foc:focdoc/*/foc:foc-name[normalize-space()=$parent-name]/.."/>
        <xsl:with-param name="file" select="$file"/>
        <xsl:with-param name="proposition-name" select="$my-proposition-name"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" select="$meth-name"/>
        <xsl:with-param name="spec" select="/descendant::foc:foc-name[normalize-space()=$parent-name]/.."/>
        <xsl:with-param name="proposition-name" select="$my-proposition-name"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="foc:identifier">
  <xsl:variable name="my_spec" 
      select="normalize-space(foc:of-species/foc:foc-name)"/>
  <xsl:variable name="self"
    select="ancestor::foc:species/foc:foc-name|ancestor::foc:collection/foc:foc-name"/>
  
  <xsl:choose>
    <xsl:when test="$my_spec=$self">
      <!--<xsl:message>
         <xsl:text>(template foc:identifier) call of find-symbol 1
         </xsl:text>
      </xsl:message>-->
      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" 
          select="normalize-space(foc:foc-name)"/>
        <xsl:with-param name="spec" 
          select="ancestor::foc:species|ancestor::foc:collection"/>
        <xsl:with-param name="proposition-name" select="concat(normalize-space(ancestor::foc:property/foc:foc-name),normalize-space(ancestor::foc:theorem/foc:foc-name))"/>
      </xsl:call-template>
    </xsl:when>
     <xsl:when test="foc:of-species">
      <!--<xsl:message>
         <xsl:text>(template foc:identifier) exists of-species
         </xsl:text>
      </xsl:message>-->
      
     <xsl:variable name="matched-param" 
       select="ancestor::foc:species/foc:parameter[@kind='collection' and normalize-space(foc:foc-name)=$my_spec]/foc:type/foc:app/foc:foc-name|ancestor::foc:collection/foc:parameter[@kind='collection' and normalize-space(foc:foc-name)=$my_spec]/foc:type/foc:app/foc:foc-name|ancestor::foc:species/foc:parameter[@kind='collection' and normalize-space(foc:foc-name)=$my_spec]/foc:type/foc:atom|ancestor::foc:collection/foc:parameter[@kind='collection' and normalize-space(foc:foc-name)=$my_spec]/foc:type/foc:atom"/>
        <!--<xsl:message>
         <xsl:text>(template foc:identifier) matched-param : </xsl:text>
         <xsl:value-of select="normalize-space($matched-param)"/>
         <xsl:value-of 
              select="normalize-space($matched-param/@infile)"/>
         <xsl:text>
         </xsl:text>
      </xsl:message>-->

     <xsl:choose>
      <xsl:when test="$matched-param/@infile">
      <!-- <xsl:message>
         <xsl:text>(foc:identifier) exists of-species OTHER FILE
         </xsl:text>
      </xsl:message>-->
      <!-- we must load another document... -->
      <xsl:variable name="myfile">
        <xsl:value-of select="concat($docpath,$matched-param/@infile)"/>
        <xsl:text>.focdoc</xsl:text>
      </xsl:variable>

      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" 
          select="normalize-space(foc:foc-name)"/>
        <xsl:with-param name="spec" select="document(string($myfile))/foc:focdoc/*/foc:foc-name[normalize-space()=normalize-space($matched-param)]/.."/>
        <xsl:with-param name="file" select="$myfile"/>
        <xsl:with-param name="proposition-name" select="concat(normalize-space(ancestor::foc:property/foc:foc-name),normalize-space(ancestor::foc:theorem/foc:foc-name))"/>
      </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <!-- we have'nt to load another document... -->
      <!--<xsl:message>
         <xsl:text>(foc:identifier) exists of-species : THIS FILE
         </xsl:text>
      </xsl:message>-->
      <xsl:call-template name="find-symbol">
        <xsl:with-param name="meth-name" 
          select="normalize-space(foc:foc-name)"/>
        <xsl:with-param name="spec" select="/descendant::foc:foc-name[normalize-space()=normalize-space($matched-param)]/.."/>
        <xsl:with-param name="proposition-name" select="concat(normalize-space(ancestor::foc:property/foc:foc-name),normalize-space(ancestor::foc:theorem/foc:foc-name))"/>
      </xsl:call-template>

      </xsl:otherwise>

      </xsl:choose>
     </xsl:when>

    <xsl:otherwise>
      <xsl:element name="mml:ci">
        <xsl:if test="foc:of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(foc:of-species)"/>
          </xsl:attribute>
          <xsl:value-of select="normalize-space(foc:of-species/foc:foc-name)"/>
          <xsl:text>!</xsl:text>
        </xsl:if>
        <xsl:value-of select="normalize-space(foc:foc-name)"/>
      </xsl:element>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- raw mathml expression -->
<xsl:template mode="addmmlnamespace" match="*">
<xsl:element name="mml:{local-name()}">
<xsl:for-each select="@*">
<xsl:attribute name="{name()}">
	  <xsl:value-of select="."/>
</xsl:attribute>
</xsl:for-each>
<xsl:apply-templates mode="addmmlnamespace" select="* | text()"/>
</xsl:element>
</xsl:template>


<xsl:template match="foc:math">
<!--NdV:faire rajouter automatiquement le ns mml. Reste a convaincre xsl
de le faire...
-->
<xsl:apply-templates mode="addmmlnamespace" select="*"/>
</xsl:template>

<xsl:template match="foc:symbol">
<xsl:choose>
<xsl:when test='name(child::foc:math)="foc:math"'>
        <xsl:apply-templates select="foc:math"/>
</xsl:when>
<xsl:otherwise>
        <xsl:apply-templates select="foc:identifier"/>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="foc:application">
<xsl:element name="mml:apply">
<xsl:apply-templates select="*"/>
</xsl:element>
</xsl:template>

 <xsl:template match="foc:implies">
 <xsl:element name="mml:apply">
 <mml:implies/>
 <xsl:apply-templates select="*"/>
</xsl:element>
 </xsl:template>

<xsl:template match="foc:equiv">
<xsl:element name="mml:apply">
 <mml:equivalent/>
 <xsl:apply-templates select="*"/>
</xsl:element>
 </xsl:template>
 
 <xsl:template match="foc:and">
 <xsl:element name="mml:apply">
 <mml:and/>
    <xsl:apply-templates select="*"/>
 </xsl:element>
 </xsl:template>


<xsl:template match="foc:or">
  <xsl:element name="mml:apply">
 <mml:or/>
     <xsl:apply-templates select="*"/>
 </xsl:element>
 </xsl:template>

<xsl:template match="foc:not">
 <xsl:element name="mml:apply">
 <mml:not/>
    <xsl:apply-templates select="*"/>
 </xsl:element>
 </xsl:template>

<xsl:template match="foc:type" mode="brother_of_var">
<!-- on se demarque des exemples de la recommandation, on suprime la
condition-->

<xsl:param name="nom" select="0"/>
<!--<xsl:message>
  <xsl:text>xsl:template match="foc:type" mode="brother_of_var"  
  </xsl:text>
</xsl:message>-->
<xsl:if test="foc:self|foc:atom|foc:prod|foc:prm|foc:fct">
 <xsl:element name="mml:apply">
<mml:in/><mml:ci><xsl:value-of select="$nom"/>
</mml:ci><xsl:apply-templates select="*" mode="math"/>
 </xsl:element>
</xsl:if>
</xsl:template>

<xsl:template match="foc:type">

<xsl:choose>
 <xsl:when test="(foc:fct or foc:atom) and (name(..)='foc:signature' or name(..)='foc:definition' or name(..)='foc:parameter' or name(..)='foc:param-prop' or name(..)='foc:carrier' or name(..)='foc:global-fun' or name(..)='foc:constr')">   
<mml:math>
  	     <xsl:choose>
	     <xsl:when test="name(..)='foc:signature' or  name(..)='foc:definition' or name(..)='foc:carrier' or name(..)='foc:global-fun' or name(..)='foc:constr'">
	        <xsl:element name="mml:apply">
                         <mml:in/>
			 <mml:ci type="function"> 
                           <xsl:value-of select="../foc:foc-name"/> 
                         </mml:ci>
			 <xsl:apply-templates select="*" mode="math"/>
		</xsl:element>
	     </xsl:when>
	     <xsl:when test="name(..)='foc:parameter' or name(..)='foc:param-prop'">
	                 <xsl:apply-templates select="*" mode="math"/> 
	     </xsl:when>
	     </xsl:choose>  

</mml:math>

</xsl:when>

<xsl:when test="(foc:self or foc:atom or foc:prm or foc:app or foc:prop or foc:tvar or foc:meth or
foc:prod) and
(name(..)='foc:signature' or  name(..)='foc:definition' or name='foc:global-fun'or name(..)='foc:constr' or name(..)='foc:parameter' or name(..)='foc:carrier' or name(..)='foc:param-prop')">
<mml:math>
<!--<xsl:choose>
<xsl:when test="name(..)='foc:signature' or  name(..)='foc:definition'">
	 <xsl:attribute name="mathcolor">salmon</xsl:attribute>
</xsl:when>
<xsl:when test="name(..)='parameter'">
	  <xsl:attribute name="mathcolor">purple</xsl:attribute>
</xsl:when>
</xsl:choose>-->  
<xsl:choose>
	<xsl:when test="ancestor::foc:parameter|ancestor::foc:param-prop">
	      <xsl:apply-templates select="*" mode="math"/>
	</xsl:when>
	<!--<xsl:when test="ancestor::foc:carrier">
		  <xsl:apply-templates select="*" mode="math"/> 
	</xsl:when> ESSAI-->
	<xsl:otherwise>
	<xsl:element name="mml:apply">
	<mml:in/>
	<mml:ci> <xsl:value-of select="../foc:foc-name"/> </mml:ci>
	 <xsl:apply-templates select="*" mode="math"/>
	 </xsl:element>
	</xsl:otherwise>
</xsl:choose> 
</mml:math>

</xsl:when>

</xsl:choose>

</xsl:template><!--type-->
 
<xsl:template match="foc:fct" mode="math">
<!--on defini par <semantic> un operateur qui associe a deux ensembles l'ensemble des fonctions de l'un vers l'autre-->
<!--<mi mathvariant="fraktur">F</mi> serait mieux pour noter les
espaces de fonction : pb des fonts non installees !!!-->
<mml:semantics>
<!--<mml:mfenced>-->
<mml:mrow>
<!--traitement special pour le premier fils-->
<xsl:choose>
               <xsl:when test="local-name(*[position()=1])='fct' or local-name(*[position()=1])='prod'">
                <mml:mfenced open="(" close=")">
  	          <xsl:apply-templates select="*[position()=1]" mode="math"/>
                </mml:mfenced>
               </xsl:when>
               <xsl:otherwise>
                <xsl:apply-templates select="*[position()=1]" mode="math"/>
               </xsl:otherwise>
              </xsl:choose>

<mml:mi>&#8594;</mml:mi>


<xsl:apply-templates select="*[position()=2]" mode="math"/>

</mml:mrow>
<!--</mml:mfenced>-->
	<mml:annotation-xml encoding="MathML-Content">
	    <mml:apply>
	     <mml:semantics>
		<!--<mrow><munder accentunder="true"><mi mathvariant="italic">FuncSpace</mi>
		<mrow><mi>dom</mi><mi>&#8594;</mi><mi>range</mi></mrow></munder></mrow>-->
                <mml:mi mathvariant="italic">FuncSpace</mml:mi>
		<mml:annotation-xml encoding="MathML-Content">
		<mml:lambda>
 		<mml:bvar><mml:ci type="set"> A </mml:ci></mml:bvar>
 		<mml:bvar><mml:ci type="set"> B </mml:ci></mml:bvar>
		<mml:set>
  			<mml:bvar><mml:ci type="function"> f </mml:ci></mml:bvar>
  			<mml:condition>
    			<mml:apply><mml:and/>
      			<mml:apply><mml:subset/>
         		<mml:apply><mml:domain/><mml:ci type="function"> f </mml:ci></mml:apply>
         		<mml:ci type="set"> A </mml:ci>
       			</mml:apply>
       			<mml:apply><mml:subset/>
         		<mml:ci type="set"> A </mml:ci>
         		<mml:apply><mml:domain/><mml:ci type="function"> f </mml:ci></mml:apply>        
       			</mml:apply>
      			<mml:apply><mml:subset/>
        		<mml:apply><mml:image/><mml:ci type="function"> f </mml:ci></mml:apply>
        		<mml:ci type="set"> B </mml:ci>
      			</mml:apply>
    			</mml:apply>
  			</mml:condition>
			<mml:ci>f</mml:ci>
 			</mml:set>
		</mml:lambda>
  		</mml:annotation-xml>
              </mml:semantics>
                 <!--<xsl:message>
                   <xsl:value-of select="local-name(*[position()=1])"/>
                   <xsl:text>
                   </xsl:text>
                   </xsl:message>-->
              <xsl:choose>
               <xsl:when test="local-name(*[position()=1])='fct' or local-name(*[position()=1])='prod'">
                 <!--<xsl:message>
                   <xsl:text> mfenced foc:fct
                   </xsl:text>
                 </xsl:message>-->
                <mml:mfenced open="(" close=")">
  	          <xsl:apply-templates select="*[position()=1]" mode="math"/>
                </mml:mfenced>
                <xsl:apply-templates select="*[position()=2]" mode="math"/>
               </xsl:when>
               <xsl:otherwise>
                <xsl:apply-templates select="*[position()=1]" mode="math"/>
                <xsl:apply-templates select="*[position()=2]" mode="math"/>
               </xsl:otherwise>
              </xsl:choose>
	   </mml:apply>
	</mml:annotation-xml>
  </mml:semantics>
</xsl:template> 
 
<xsl:template match="foc:prod" mode="math">
<mml:apply>
<mml:cartesianproduct/>
  <xsl:choose>
              <xsl:when test="local-name(*[position()=1])='fct' or local-name(*[position()=1])='prod'">
               <!--<xsl:message>
                   <xsl:text> mfenced foc:prod
                   </xsl:text>
                 </xsl:message>-->
                <mml:mfenced open="(" close=")">
  	          <xsl:apply-templates select="*[position()=1]" mode="math"/>
                </mml:mfenced>
                <xsl:apply-templates select="*[position()=2]" mode="math"/>
               </xsl:when>
               <xsl:otherwise>
                <xsl:apply-templates select="*[position()=1]" mode="math"/>
                <xsl:apply-templates select="*[position()=2]" mode="math"/>
               </xsl:otherwise>
  </xsl:choose>
</mml:apply>
</xsl:template>


 <xsl:template match="foc:self" mode="math" >
  <mml:ci>
	<xsl:text>self</xsl:text>
	<!--<xsl:if test="@order='high'"><xsl:text>!</xsl:text></xsl:if>-->
 </mml:ci>
 </xsl:template>

<xsl:template match="foc:atom" mode="math">
 <mml:ci>
	<xsl:value-of select="."/>
	<!--<xsl:if test="@order='high'"><xsl:text>!</xsl:text></xsl:if>-->
</mml:ci>
 </xsl:template>

<xsl:template match="foc:prop" mode="math" >
 <mml:ci>prop</mml:ci>
 </xsl:template>

<xsl:template match="foc:tvar" mode="math" >
 <mml:ci><xsl:value-of select="."/></mml:ci>
 </xsl:template>

<xsl:template match="foc:prm" mode="math">
   <xsl:element name="mml:apply">
    <mml:ci type="function"><xsl:value-of select="normalize-space(foc:foc-name)"/></mml:ci>
    <xsl:apply-templates select="*[position()!=last()]" mode="math"/>
   </xsl:element>
  </xsl:template>

 <xsl:template match="foc:app" mode="math">
 <xsl:choose>
 <xsl:when test="ancestor::foc:parameter">
 <mml:apply>
    <mml:ci type="collection"><xsl:value-of select="normalize-space(foc:foc-name)"/></mml:ci>
     <xsl:for-each select="foc:param">
			  <mml:ci> <xsl:value-of select="normalize-space(.)"/></mml:ci>
	   </xsl:for-each>
 </mml:apply>
 </xsl:when>
 <xsl:otherwise>
<!-- methode d'ordre sup, on veut la collection et non un element de
 la collection (sinon on aurait utilise prm)-->
<mml:set>
	<mml:apply>
	  <mml:ci type="collection">
	  <xsl:value-of select="normalize-space(foc:foc-name)"/>
	  </mml:ci>
          <xsl:for-each select="param">
			  <mml:ci> <xsl:value-of select="normalize-space(.)"/></mml:ci>
	  </xsl:for-each>
	</mml:apply>
</mml:set>
 </xsl:otherwise>
 </xsl:choose>
   </xsl:template>

 <xsl:template match="foc:meth" mode="math">
    	<mml:apply>
		<mml:ci type="function">methode</mml:ci>
			<xsl:apply-templates select="*[position()!=last()]" mode="math"/>
		<mml:ci>	     
		<xsl:value-of select="normalize-space(foc:foc-name)"/>
		</mml:ci>
	</mml:apply>
  </xsl:template>

</xsl:stylesheet>

