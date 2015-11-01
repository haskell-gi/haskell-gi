/*
  Compute the number of bytes required for storage of a given enum,
  assuming that the current compiler gives the same result as the
  compiler used for compiling the library being introspected.

  Adapted from girepository/giroffsets.c, in the gobject-introspection
   distribution. Original copyright below.
*/

/* -*- mode: C; c-file-style: "gnu"; indent-tabs-mode: nil; -*-
 * GObject introspection: Compute structure offsets
 *
 * Copyright (C) 2008 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* The C standard specifies that an enumeration can be any char or any signed
 * or unsigned integer type capable of representing all the values of the
 * enumeration. We use test enumerations to figure out what choices the
 * compiler makes. (Ignoring > 32 bit enumerations)
 */

#include <glib.h>

typedef enum {
  ENUM_1 = 1 /* compiler could use int8, uint8, int16, uint16, int32, uint32 */
} Enum1;

typedef enum {
  ENUM_2 = 128 /* compiler could use uint8, int16, uint16, int32, uint32 */
} Enum2;

typedef enum {
  ENUM_3 = 257 /* compiler could use int16, uint16, int32, uint32 */
} Enum3;

typedef enum {
  ENUM_4 = G_MAXSHORT + 1 /* compiler could use uint16, int32, uint32 */
} Enum4;

typedef enum {
  ENUM_5 = G_MAXUSHORT + 1 /* compiler could use int32, uint32 */
} Enum5;

typedef enum {
  ENUM_6 = ((guint)G_MAXINT) + 1 /* compiler could use uint32 */
} Enum6;

typedef enum {
  ENUM_7 = -1 /* compiler could use int8, int16, int32 */
} Enum7;

typedef enum {
  ENUM_8 = -129 /* compiler could use int16, int32 */
} Enum8;

typedef enum {
  ENUM_9 = G_MINSHORT - 1 /* compiler could use int32 */
} Enum9;

int
_gi_get_enum_storage_bytes (gint64 min_value, gint64 max_value)
{
  int width;

  if (min_value < 0)
    {
      if (min_value > -128 && max_value <= 127)
	width = sizeof(Enum7);
      else if (min_value >= G_MINSHORT && max_value <= G_MAXSHORT)
	width = sizeof(Enum8);
      else
	width = sizeof(Enum9);
    }
  else
    {
      if (max_value <= 127)
	{
	  width = sizeof (Enum1);
	}
      else if (max_value <= 255)
	{
	  width = sizeof (Enum2);
	}
      else if (max_value <= G_MAXSHORT)
	{
	  width = sizeof (Enum3);
	}
      else if (max_value <= G_MAXUSHORT)
	{
	  width = sizeof (Enum4);
	}
      else if (max_value <= G_MAXINT)
	{
	  width = sizeof (Enum5);
	}
      else
	{
	  width = sizeof (Enum6);
	}
    }

  if (width == 1 || width == 2 || width == 4 || width == 8) {
    return width;
  } else {
    g_error("Unexpected enum width %d", width);
  }
}
