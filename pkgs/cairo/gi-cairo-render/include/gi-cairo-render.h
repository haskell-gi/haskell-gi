#include<cairo.h>
#define CAIRO_CHECK_VERSION(major,minor,micro)    \
        (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(major,minor,micro))
#ifdef CAIRO_HAS_PDF_SURFACE
#include<cairo-pdf.h>
#endif
#ifdef CAIRO_HAS_PS_SURFACE
#include<cairo-ps.h>
#endif
#ifdef CAIRO_HAS_SVG_SURFACE
#include<cairo-svg.h>
#endif

