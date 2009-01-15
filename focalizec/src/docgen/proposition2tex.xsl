<?xml version="1.0" encoding="utf-8"?>
<!-- FoCDoc to HTML (MathML). Taggs proposition and descendants -->

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:f="focdoc.dtd"
  version="1.0">
 <xsl:output method="text"
    indent="yes"
    doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
    doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
<xsl:param name="verbose" select="0"/>

<!-- Variable qui determine s'il faut ecrire reln ou apply, il faudra sans doute
     ajouter des relations a la liste. Pour cela concatener
     $(liste_reln)_nomDeLaNouvelleRelation_-->
<xsl:variable
   name="liste_reln">_implies_different_leq_geq_lt_gt_equal_</xsl:variable>
<xsl:variable
   name="liste_operateur">_plus_mult_div_minus_gcd_opposite_remainder_quotient_</xsl:variable>

<!--  utile pour les tests :
 <xsl:template match="/">
  <root>
 <xsl:apply-templates select="//proposition"/>
 </root>
  </xsl:template>-->

<!--
<xsl:template match="focdoc">
<xsl:text>

</xsl:text>
<xsl:text>NB : </xsl:text><math:math mode="inline">
<mstyle color="red">
 <mrow><mi mathvariant="italic">FuncSpace</mi><mi> : </mi><mi>A :set, B : set </mi><mi>&#8594;</mi>
                        <set>
                        <bvar><ci type="function"> f </ci></bvar>
                        <condition>
                        <apply><and/>
                        <apply><subset/>
                        <apply><domain/><ci type="function"> f </ci></apply>
                        <ci type="set"> A </ci>
                        </apply>
                        <apply><subset/>
                        <ci type="set"> A </ci>
                        <apply><domain/><ci type="function"> f </ci></apply>        
                        </apply>
                        <apply><subset/>
                        <apply><image/><ci type="function"> f </ci></apply>
                        <ci type="set"> B </ci>
                        </apply>
                        </apply>
                        </condition>
                        <ci>f</ci>
                        </set></mrow>   
</mstyle>
  </math:math>
</xsl:template>
-->



<xsl:template match="int">
  <xsl:value-of select="@val"/>
</xsl:template>



<xsl:template match="string">
  <xsl:text>"</xsl:text>
  <xsl:value-of select="@val"/>
  <xsl:text>"</xsl:text>
</xsl:template>



<xsl:template match="proposition">
  <xsl:param name="pere" select="normalize-space(../../foc-name)"/>
  <xsl:param name="descendant"
             select="normalize-space(../history/first)"/><!--
    <xsl:when test="contains(concat(concat('_',$descendant),'_'),concat(concat('_',$pere),'_'))">
    verification de la possibilite d'ecrire des egalites de string
    -->
  <xsl:if test = "$descendant=$pere or $verbose >= 1">
    <xsl:text>$$</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>$$</xsl:text>
  </xsl:if>
</xsl:template>



<!-- traiter exists de la meme facon.-->
<xsl:template match="f:all">
  <xsl:text>\forall </xsl:text>
  <xsl:apply-templates select="*[name()!='type']"/>
</xsl:template>


 
<xsl:template match="f:ex">
  <xsl:text>\exists </xsl:text>
  <xsl:apply-templates select="*[name()!='type']"/>
</xsl:template>

<xsl:template match="f:var">
  <xsl:param name="nom">
    <xsl:value-of select="normalize-space(foc-name)"/>
  </xsl:param>
  <xsl:choose>
    <xsl:when test="name(..)='f:all' or name(..)=f:ex">
      <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
          <xsl:value-of select="normalize-space(foc-name)"/>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="following-sibling::type[position()=1]" 
                           mode="brother_of_var">
        <xsl:with-param name="nom" select="$nom"/>
      </xsl:apply-templates>
      <xsl:text>.\ </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
          <xsl:value-of select="normalize-space(foc-name)"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!--pour l'instant je ne traite pas les of-species-->
<xsl:template match="identifier">
  <xsl:call-template name="escape-underscore">
    <xsl:with-param name="x">
      <xsl:value-of select="normalize-space(foc-name)"/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<!-- raw mathml expression -->
<!-- NdV trouver une solution pour traiter a la fois mathml et tex... -->
<!--
<xsl:template match="math:math">
    <xsl:copy-of select="*"/>
</xsl:template>
-->
<xsl:template match="f:symbol">
  <xsl:choose>
    <xsl:when test="latex">
      <xsl:value-of select="latex"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="identifier"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="application">
<!--NdV Il devrait y avoir un traitement particulier, mais c'est plus difficile
    a realiser que pour mathml, car le passage aux infixes est a faire a la 
    main
-->
<xsl:choose>
  <xsl:when 
     test="./child::f:symbol[position()=1]/latex[@infix='true']">
    <xsl:apply-templates select="*[position()=2]"/>
    <xsl:apply-templates select="*[position()=1]"/>
    <xsl:apply-templates select="*[position()=3]"/>
  </xsl:when>
  <xsl:otherwise>
    <xsl:apply-templates select="*[position()=1]"/>
    <xsl:text>(</xsl:text>
    <xsl:for-each select="*[position()>1]">
      <xsl:apply-templates select="."/>
      <xsl:if test="position()!=last()">
        <xsl:text>,</xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:otherwise>
</xsl:choose>  
</xsl:template>



<!-- le premier fils d'un tag application est un identifier-->
<!--NdV: First son is not necessarily an identifier... -->
<!-- NdV: pas utilise pour l'instant -->
<xsl:template match="identifier" mode="operateur">
  <xsl:param name="p" select="0"/>
  <!-- <xsl:attribute name="name"><xsl:value-of select="$p"/></xsl:attribute> pour ajouter un attribut apres un element-->
  <xsl:choose>
    <!--concat parcqu'il n'y a pas d'egalite sur les strings, juste sur les sous chaines-->
    <xsl:when test="contains($liste_reln ,concat(concat('_',$p),'_'))">
      <!-- normalement c'est <reln> si on respecte mathml-->
      <!--<xsl:element name="{$p}"/>-->
      <xsl:call-template name="transformation">
        <xsl:with-param name="p" select="$p"/>
      </xsl:call-template>
    </xsl:when>
    <!--<xsl:when test="contains($liste_operateur ,concat(concat('_',normalize-space(foc-name)),'_'))">-->
    <xsl:when test="contains($liste_operateur ,concat(concat('_',$p),'_'))">
      <!--<xsl:element name="{$p}"/>-->
      <xsl:call-template name="transformation">
        <xsl:with-param name="p" select="$p"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <!-- ni operateur connu par mathml ni relation identifiee par mathml-->
      <ci>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:value-of select="normalize-space($p)"/>
      </ci>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- NdV: probleme du parenthesage... -->
<xsl:template match="f:implies">
  <!--<reln> normalement, si on respectait mathml 1.0 mais deprecie par 2.0...-->
  <xsl:variable name="p">
    <xsl:if test="*[position()=1 and (all|ex|and|or|not)]">
      <xsl:text>y</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:if test="$p='y'">
    <xsl:text>(</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="*[position()=1]"/>
  <xsl:if test="$p='y'">
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>\rightarrow </xsl:text>
  <xsl:apply-templates select="*[position()=2]"/>
</xsl:template>



<xsl:template match="f:and">
  <xsl:variable name="p">
    <xsl:if test="*[position()=1 and (all|ex|implies|or|not)]">
      <xsl:text>y</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:if test="$p='y'">
    <xsl:text>(</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="*[position()=1]"/>
  <xsl:if test="$p='y'">
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>\wedge </xsl:text>
  <xsl:apply-templates select="*[position()=2]"/>
</xsl:template>



<xsl:template match="f:or">
  <xsl:variable name="p">
    <xsl:if test="*[position()=1 and (all|ex|implies|and|not)]">
      <xsl:text>y</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:if test="$p='y'">
    <xsl:text>(</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="*[position()=1]"/>
  <xsl:if test="$p='y'">
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>\vee </xsl:text>
  <xsl:apply-templates select="*[position()=2]"/>
</xsl:template>



 <!-- <xsl:apply-templates select="all|and|application|atom|foc-name|implies|not|or|prm|self|type|var"/>-->
 <xsl:template match="f:not">
   <xsl:text>\neg </xsl:text>
   <xsl:apply-templates select="*"/>
 </xsl:template>



 <xsl:template match="type" mode="brother_of_var">
   <xsl:param name="nom" select="0"/>
   <!--<xsl:if test="name(..)='all' or name(..)='ex'">-->
   <xsl:if test="self|atom">
     <xsl:choose>
       <xsl:when test="atom">
         <xsl:call-template name="escape-underscore">
           <xsl:with-param name="x">
             <xsl:value-of select="normalize-space(atom)"/>
           </xsl:with-param>
         </xsl:call-template>
       </xsl:when>
       <xsl:when test="self">
         <xsl:text>\in \self</xsl:text>
       </xsl:when>
     </xsl:choose>
   </xsl:if>
 </xsl:template>



<!--NdV: a priori pas utilise -->
<!--ce template transforme mult en times, equal en eq, different en neq... Bref, on veut transformer les noms des fonctions foc en fonctions mathml. D'ailleurs ca aurait ete plus simple de donner des noms universels des le depart, non ?-->
<!-- on recupere ici des operateurs connus ou des relations-->
<xsl:template name="transformation">
  <!-- on recupere le parametre-->
  <xsl:param name="p" select="0"/>
  <xsl:choose>
    <!--important ces "underscores": se souvenir qu'on a des operateurs comme module_mult-->
    <xsl:when test="contains('_mult_' ,concat(concat('_',$p),'_'))">
      <times>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </times>
    </xsl:when>
    <xsl:when test="contains('_equal_' ,concat(concat('_',$p),'_'))">
      <eq>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </eq>
    </xsl:when>
    <xsl:when test="contains('_different_' ,concat(concat('_',$p),'_'))">
      <neq>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </neq>
    </xsl:when>
    <xsl:when test="contains('_leq_' ,concat(concat('_',$p),'_'))">
      <leq>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </leq>
    </xsl:when>
    <xsl:when test="contains('_lt_' ,concat(concat('_',$p),'_'))">
      <lt>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </lt>
    </xsl:when>
    <xsl:when test="contains('_gt_' ,concat(concat('_',$p),'_'))">
      <gt>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </gt>
    </xsl:when>
    <xsl:when test="contains('_geq_' ,concat(concat('_',$p),'_'))">
      <geq>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </geq>
    </xsl:when>
    <xsl:when test="contains('_remainder_' ,concat(concat('_',$p),'_'))">
      <rem>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </rem>
    </xsl:when>
    <xsl:when test="contains('_quotient_' ,concat(concat('_',$p),'_'))">
      <quo>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </quo>
    </xsl:when>
    <xsl:when test="contains('_opposite_' ,concat(concat('_',$p),'_'))">
      <minus>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </minus>
    </xsl:when>
    <xsl:when test="contains('_minus_' ,concat(concat('_',$p),'_'))">
      <minus>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </minus>
    </xsl:when>
    <xsl:when test="contains('_plus_' ,concat(concat('_',$p),'_'))">
      <plus>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </plus>
    </xsl:when>
    <xsl:when test="contains('_div_' ,concat(concat('_',$p),'_'))">
      <divide>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </divide>
    </xsl:when>
    <xsl:when test="contains('_gcd_' ,concat(concat('_',$p),'_'))">
      <gcd>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
      </gcd>
    </xsl:when>
    <xsl:otherwise>
      <!--<xsl:element name="{$p}"/> pour recuperer le nom de l'element-->
      <!--normalement on ne doit pas arriver jusqu'ici-->
      <ci>
        <xsl:if test="of-species">
          <xsl:attribute name="type">
            <xsl:value-of select="normalize-space(of-species)"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:value-of select="normalize-space($p)"/>
      </ci>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- On arrive ici par apply-import depuis focdoc2html -->
<xsl:template match="type">
  <xsl:choose>
    <!--Ivan 21/07-->
    <xsl:when test="fct and (name(..)='signature' or name(..)='definition')">
      <xsl:text>: $ </xsl:text>
      <xsl:apply-templates select="*" mode="math"/>
      <xsl:text>$</xsl:text>
    </xsl:when>

    <xsl:when test="(self or atom) and name(..)='signature'">
      <xsl:text>: $</xsl:text>
      <xsl:apply-templates select="*" mode="math"/>
      <xsl:text>$</xsl:text>
    </xsl:when>

  </xsl:choose>
  </xsl:template><!--type-->



<!--Ivan 21/07-->
<xsl:template  match="fct" mode="math">
  <xsl:variable name="p">
    <xsl:if test="*[position()=1 and (fct|prod)]">
      <xsl:text>y</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:if test="$p='y'">
    <xsl:text>(</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="*[position()=1]" mode="math"/>
  <xsl:if test="$p='y'">
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>\rightarrow </xsl:text>
  <xsl:apply-templates select="*[position()=2]" mode="math"/>
</xsl:template> 



<!--Ivan 21/07-->
<xsl:template match="prod" mode="math">
  <xsl:variable name="p">
    <xsl:if test="*[position()=1 and (fct|prod)]">
      <xsl:text>y</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:if test="$p='y'">
    <xsl:text>(</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="*[position()=1]" mode="math"/>
  <xsl:if test="$p='y'">
    <xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:text>* </xsl:text>
  <xsl:apply-templates select="*[position()=2]" mode="math"/>
</xsl:template>



<!--Ivan 21/07-->
<xsl:template match="self" mode="math" >
  <xsl:text>\selfmath</xsl:text>
</xsl:template>



<!--Ivan 21/07-->
<xsl:template match="atom" mode="math" >
  <xsl:text>\typename{</xsl:text>
  <xsl:call-template name="escape-underscore">
    <xsl:with-param name="x">
      <xsl:value-of select="."/>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:text>}</xsl:text>
</xsl:template>


<!-- _ -> \_ dans les identificateurs passes a latex. -->
<xsl:template name="escape-underscore">
  <xsl:param name="x"/>
  <xsl:variable name="prefix">
    <xsl:value-of select="substring-before($x,'_')"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$prefix=''">
      <!-- no underscore: we can safely return x -->
      <xsl:value-of select="$x"/> 
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="postfix">
        <xsl:value-of select="substring-after($x,'_')"/>
      </xsl:variable>
      <xsl:value-of select="$prefix"/>
      <xsl:text>\_</xsl:text>
      <xsl:call-template name="escape-underscore">
        <xsl:with-param name="x">
          <xsl:value-of select="$postfix"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
