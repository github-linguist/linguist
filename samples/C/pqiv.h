/**
 * pqiv
 *
 * Copyright (c) 2013-2014, Phillip Berndt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

// This file contains the definition of files, image types and
// the plugin infrastructure. It should be included in file type
// handlers.

#ifndef _PQIV_H_INCLUDED
#define _PQIV_H_INCLUDED

#include <glib.h>
#include <gtk/gtk.h>
#include <gio/gio.h>
#include "lib/bostree.h"

#ifndef PQIV_VERSION
#define PQIV_VERSION "2.3"
#endif

#define FILE_FLAGS_ANIMATION      (guint)(1)
#define FILE_FLAGS_MEMORY_IMAGE   (guint)(1<<1)

// The structure for images {{{
typedef struct file_type_handler_struct_t file_type_handler_t;
typedef struct {
	// File type
	const file_type_handler_t *file_type;

	// Special flags
	// FILE_FLAGS_ANIMATION        -> Animation functions are invoked
	//                                Set by file type handlers
	// FILE_FLAGS_MEMORY_IMAGE     -> File lives in memory
	guint file_flags;

	// The file name to display and to sort by
	gchar *display_name;

	// The URI or file name of the file
	gchar *file_name;

	// If the file is a memory image, the actual image data
	GBytes *file_data;

	// The file monitor structure is used for inotify-watching of
	// the files
	GFileMonitor *file_monitor;

	// This flag stores whether this image is currently loaded
	// and valid. i.e. if it is set, you can assume that
	// private_data contains a representation of the image;
	// if not, you can NOT assume that it does not.
	gboolean is_loaded;

	// Cached image size
	guint width;
	guint height;

	// File-type specific data, allocated and freed by the file type handlers
	void *private;
} file_t;
// }}}
// Definition of the built-in file types {{{

// If you want to implement your own file type, you'll have to implement the
// following functions and a non-static initialization function named
// file_type_NAME_initializer that fills a file_type_handler_t with pointers to
// the functions. Store the file in backends/NAME.c and adjust the Makefile to
// add the required libraries if your backend is listed in the $(BACKENDS)
// variable.

typedef enum { PARAMETER, RECURSION, INOTIFY, BROWSE_ORIGINAL_PARAMETER, FILTER_OUTPUT } load_images_state_t;
// Allocation function: Allocate the ->private structure within a file and add the
// image(s) to the list of available images via load_images_handle_parameter_add_file()
// If an image is not to be loaded for any reason, the file structure should be
// deallocated using file_free()
// Returns a pointer to the first added image
// Optional, you can also set the pointer to this function to NULL.
typedef BOSNode *(*file_type_alloc_fn_t)(load_images_state_t state, file_t *file);

// Deallocation, if a file is removed from the images list. Free the ->private structure.
// Only called if ->private is non-NULL.
typedef void (*file_type_free_fn_t)(file_t *file);

// Actually load a file into memory
typedef void (*file_type_load_fn_t)(file_t *file, GInputStream *data, GError **error_pointer);

// Unload a file
typedef void (*file_type_unload_fn_t)(file_t *file);

// Animation support: Initialize memory for animations, return ms until first frame
// Optional, you can also set the pointer to this function to NULL.
typedef double (*file_type_animation_initialize_fn_t)(file_t *file);

// Animation support: Advance to the next frame, return ms until next frame
// Optional, you can also set the pointer to this function to NULL.
typedef double (*file_type_animation_next_frame_fn_t)(file_t *file);

// Draw the current view to a cairo context
typedef void (*file_type_draw_fn_t)(file_t *file, cairo_t *cr);

struct file_type_handler_struct_t {
	// All files will be filtered with this filter. If it lets it pass,
	// a handler is assigned to a file. If none do, the file is
	// discarded if it was found during directory traversal or
	// loaded using the first image backend if it was an explicit
	// parameter.
	GtkFileFilter *file_types_handled;

	// Pointers to the functions defined above
	file_type_alloc_fn_t alloc_fn;
	file_type_free_fn_t free_fn;
	file_type_load_fn_t load_fn;
	file_type_unload_fn_t unload_fn;
	file_type_animation_initialize_fn_t animation_initialize_fn;
	file_type_animation_next_frame_fn_t animation_next_frame_fn;
	file_type_draw_fn_t draw_fn;
};

// Initialization function: Tell pqiv about a backend
typedef void (*file_type_initializer_fn_t)(file_type_handler_t *info);

// pqiv symbols available to plugins {{{

// Global cancellable that should be used for every i/o operation
extern GCancellable *image_loader_cancellable;

// Current scale level. For backends that don't support cairo natively.
extern gdouble current_scale_level;

// Load a file from disc/memory/network
GInputStream *image_loader_stream_file(file_t *file, GError **error_pointer);

// Add a file to the list of loaded files
// Should be called at least once in a file_type_alloc_fn_t, with the state being
// forwarded unaltered.
BOSNode *load_images_handle_parameter_add_file(load_images_state_t state, file_t *file);

// Load all data from an input stream into memory, conveinience function
GBytes *g_input_stream_read_completely(GInputStream *input_stream, GCancellable *cancellable, GError **error_pointer);

// Free a file
void file_free(file_t *file);

// }}}

// File type handlers, used in the initializer and file type guessing
extern file_type_handler_t file_type_handlers[];

/* }}} */

#endif
