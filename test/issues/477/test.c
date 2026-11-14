/* C side of the regression test for the struct offset problem in
   https://github.com/haskell-gi/haskell-gi/issues/477
   (see https://github.com/haskell-gi/haskell-gi/issues/477#issuecomment-3508045340)

   GstVideoFrame embeds a GstVideoInfo, which ends in an anonymous
   union. The typelib ignores that union, so it gets the size of
   GstVideoInfo wrong (120 bytes instead of 152 on x86_64), and the
   offsets of every field of GstVideoFrame after "info" were 32 bytes
   too small.

   The functions here map frames (for Haskell to read), and check
   frames written from Haskell, always going through the C compiler's
   idea of the layout. */

#include <string.h>
#include <gst/video/video.h>

#define WIDTH 64
#define HEIGHT 48

/* Map a newly allocated RGBA buffer into the given (zeroed) frame,
   allocated from Haskell, and then set some of the fields of the
   frame to known values. Returns the mapped buffer (the frame keeps a
   reference to it, until it is unmapped), or NULL on failure. */
GstBuffer *
issue477_map_frame (GstVideoFrame *frame)
{
  GstVideoInfo info;
  GstBuffer *buffer;
  gboolean mapped;

  gst_video_info_init (&info);
  if (!gst_video_info_set_format (&info, GST_VIDEO_FORMAT_RGBA, WIDTH, HEIGHT))
    return NULL;

  buffer = gst_buffer_new_allocate (NULL, GST_VIDEO_INFO_SIZE (&info), NULL);
  mapped = gst_video_frame_map (frame, &info, buffer, GST_MAP_READ);
  /* The frame holds its own reference to the buffer. */
  gst_buffer_unref (buffer);
  if (!mapped)
    return NULL;

  frame->flags = GST_VIDEO_FRAME_FLAG_INTERLACED | GST_VIDEO_FRAME_FLAG_TFF;
  frame->id = 42;

  return buffer;
}

/* Whether the fields of the frame have the given values. */
gboolean
issue477_frame_equals (const GstVideoFrame *frame, GstBuffer *buffer,
                       gint flags, gint id)
{
  return frame->buffer == buffer && (gint) frame->flags == flags
    && frame->id == id;
}

/* Whether the GstVideoInfo embedded in the frame is the one set up
   by gst_video_frame_map() for a WIDTH x HEIGHT RGBA frame. */
gboolean
issue477_frame_info_is_mapped (const GstVideoFrame *frame)
{
  return GST_VIDEO_INFO_FORMAT (&frame->info) == GST_VIDEO_FORMAT_RGBA
    && GST_VIDEO_INFO_WIDTH (&frame->info) == WIDTH
    && GST_VIDEO_INFO_HEIGHT (&frame->info) == HEIGHT;
}

/* Whether the GstVideoInfo embedded in the frame is all zeros. */
gboolean
issue477_frame_info_is_zero (const GstVideoFrame *frame)
{
  GstVideoInfo zero;

  memset (&zero, 0, sizeof zero);
  return memcmp (&frame->info, &zero, sizeof zero) == 0;
}
