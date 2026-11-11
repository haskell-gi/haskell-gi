/* C side of the regression test for
   https://github.com/haskell-gi/haskell-gi/issues/485

   The functions here either build PangoGlyphStrings with known
   contents (for Haskell to read), or check that structs written from
   Haskell have exactly the same contents as the equivalent structs
   written from C (compared byte by byte, so wrong offsets or stray
   bits are detected too). */

#include <string.h>
#include <pango/pango.h>

/* The contents of glyph number i in the glyph strings we build. */
static void
fill_glyph (PangoGlyphInfo *info, int i)
{
  memset (info, 0, sizeof (PangoGlyphInfo));
  info->glyph = 1000 + i;
  info->geometry.width = 1024 * (i + 1);
  info->geometry.x_offset = 10 * i;
  info->geometry.y_offset = -10 * i;
  info->attr.is_cluster_start = (i % 2 == 0);
  info->attr.is_color = (i % 3 == 0);
}

/* A glyph string with n glyphs, glyph i filled in by fill_glyph(). */
PangoGlyphString *
issue485_make_glyph_string (int n)
{
  PangoGlyphString *string = pango_glyph_string_new ();

  pango_glyph_string_set_size (string, n);
  for (int i = 0; i < n; i++)
    {
      fill_glyph (&string->glyphs[i], i);
      string->log_clusters[i] = 2 * i;
    }

  return string;
}

/* A newly allocated array of n glyphs, filled in by fill_glyph(). It
   should be freed with g_free(). */
PangoGlyphInfo *
issue485_make_glyphs (int n)
{
  PangoGlyphInfo *glyphs = g_new (PangoGlyphInfo, n);

  for (int i = 0; i < n; i++)
    fill_glyph (&glyphs[i], i);

  return glyphs;
}

/* Whether the given glyph has the contents set by fill_glyph(). */
gboolean
issue485_glyph_has_pattern (const PangoGlyphInfo *info, int i)
{
  PangoGlyphInfo expected;

  fill_glyph (&expected, i);
  return memcmp (info, &expected, sizeof (PangoGlyphInfo)) == 0;
}

/* Whether the glyph string has n glyphs, filled in by fill_glyph(). */
gboolean
issue485_glyph_string_has_pattern (const PangoGlyphString *string, int n)
{
  if (string->num_glyphs != n)
    return FALSE;

  for (int i = 0; i < n; i++)
    if (!issue485_glyph_has_pattern (&string->glyphs[i], i))
      return FALSE;

  return TRUE;
}

gboolean
issue485_vis_attr_equals (const PangoGlyphVisAttr *attr,
                          guint is_cluster_start, guint is_color)
{
  PangoGlyphVisAttr expected;

  memset (&expected, 0, sizeof expected);
  expected.is_cluster_start = is_cluster_start;
  expected.is_color = is_color;
  return memcmp (attr, &expected, sizeof expected) == 0;
}

gboolean
issue485_geometry_equals (const PangoGlyphGeometry *geometry,
                          int width, int x_offset, int y_offset)
{
  PangoGlyphGeometry expected;

  memset (&expected, 0, sizeof expected);
  expected.width = width;
  expected.x_offset = x_offset;
  expected.y_offset = y_offset;
  return memcmp (geometry, &expected, sizeof expected) == 0;
}

/* Whether the glyph has the given glyph id, and zeros elsewhere. */
gboolean
issue485_glyph_info_equals (const PangoGlyphInfo *info, guint glyph)
{
  PangoGlyphInfo expected;

  memset (&expected, 0, sizeof expected);
  expected.glyph = glyph;
  return memcmp (info, &expected, sizeof expected) == 0;
}
