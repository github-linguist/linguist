/*
 * Copyright (c) 2012 Dave Vasilevsky <dave@vasilevsky.ca>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#pragma once

#include "common.h"

#include "squashfs_fs.h"

typedef struct {
  sqfs_md_cursor cur;
  sqfs_off_t offset, total;
  struct squashfs_dir_header header;
} sqfs_dir;

typedef struct {
  sqfs_inode_id inode;
  sqfs_inode_num inode_number;
  int type;
  char *name;
  size_t name_size;
  sqfs_off_t offset, next_offset;
} sqfs_dir_entry;

typedef char sqfs_name[SQUASHFS_NAME_LEN + 1];

/* Begin a directory traversal, initializing the dir structure.
   If offset is non-zero, fast-forward to that offset in the directory. */
sqfs_err sqfs_dir_open(sqfs *fs, sqfs_inode *inode, sqfs_dir *dir,
                       off_t offset);

/* Initialize a dir_entry structure before use.
        'namebuf' should be a character buffer of enough size to hold any name,
        see sqfs_name. It may also be NULL, in which case no names will be
   placed
        into this dir_entry. */
void sqfs_dentry_init(sqfs_dir_entry *entry, char *namebuf);

/* Get the next directory entry, filling in the dir_entry.
         Returns false when out of entries, or on error. */
bool sqfs_dir_next(sqfs *fs, sqfs_dir *dir, sqfs_dir_entry *entry,
                   sqfs_err *err);

/* Lookup an entry in a directory inode.
         The dir_entry must have been initialized with a buffer. */
sqfs_err sqfs_dir_lookup(sqfs *fs, sqfs_inode *inode, const char *name,
                         size_t namelen, sqfs_dir_entry *entry, bool *found);

/* Lookup a complete path, and replace *inode with the results.
         Uses / (slash) as the directory separator. */
sqfs_err sqfs_lookup_path(sqfs *fs, sqfs_inode *inode, const char *path,
                          bool *found);

/* Accessors on sqfs_dir_entry */
sqfs_off_t sqfs_dentry_offset(sqfs_dir_entry *entry);
sqfs_off_t sqfs_dentry_next_offset(sqfs_dir_entry *entry);
int sqfs_dentry_type(sqfs_dir_entry *entry);
sqfs_mode_t sqfs_dentry_mode(sqfs_dir_entry *entry);
sqfs_inode_id sqfs_dentry_inode(sqfs_dir_entry *entry);
sqfs_inode_num sqfs_dentry_inode_num(sqfs_dir_entry *entry);
size_t sqfs_dentry_name_size(sqfs_dir_entry *entry);
bool sqfs_dentry_is_dir(sqfs_dir_entry *entry);

/* Yields the name of this directory entry, or NULL if the dir_entry structure
   was initialized without a name buffer. Name will be nul-terminated. */
const char *sqfs_dentry_name(sqfs_dir_entry *entry);
