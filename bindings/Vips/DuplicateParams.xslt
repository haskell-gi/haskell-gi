<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:gi="http://www.gtk.org/introspection/core/1.0">

<xsl:output method="text"/>
    <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'" />
    <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

<!--
  Several libvips functions contain parameters having the same names, except
  in a different case, e.g. vips_col_sRGB2scRGB_8(int r, int g, int b, float *R, float *G, float *B).
  In Haskell, all params are lowercase, resulting in duplicate params.
  Fix: replace all function parameters comprising a single uppercase letter to
  include an underscore suffix.
-->
<xsl:template match="/">
  <xsl:for-each select="//gi:function//gi:parameter[contains($uppercase, @name)]">
    <xsl:text>set-attr </xsl:text>
    <xsl:apply-templates select="ancestor::*"/>
    <xsl:apply-templates select="."/>
    <xsl:text> name </xsl:text>
    <xsl:value-of select="@name"/><xsl:text>_</xsl:text>
    <xsl:text>&#xa;</xsl:text>
  </xsl:for-each>>
</xsl:template>

<xsl:template match="gi:repository">
</xsl:template>

<xsl:template match="*[@name!='']">
  <xsl:text>/</xsl:text>
  <xsl:value-of select="@name"/>
</xsl:template>

<xsl:template match="*[name(..)='repository']">
  <xsl:value-of select="@name"/>
</xsl:template>

<xsl:template match="*">
  <xsl:text>/@</xsl:text>
  <xsl:value-of select="name()"/>
</xsl:template>

</xsl:stylesheet>
