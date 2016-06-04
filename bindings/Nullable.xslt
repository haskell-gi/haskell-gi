<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:gi="http://www.gtk.org/introspection/core/1.0">

<xsl:output method="text"/>

<xsl:template match="/">
  <xsl:for-each select="//*[@nullable!='']">
      <xsl:text>set-attr </xsl:text>
      <xsl:apply-templates select="ancestor::*"/>
      <xsl:apply-templates select="."/>
      <xsl:text> nullable </xsl:text>
      <xsl:value-of select="@nullable"/>
      <xsl:text>&#xa;</xsl:text>
  </xsl:for-each>
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
