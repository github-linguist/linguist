/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_settings_h__
#define INCLUDE_settings_h__

extern int git_settings_global_init(void);

extern const char *git_settings__user_agent(void);
extern const char *git_settings__user_agent_product(void);

#endif
