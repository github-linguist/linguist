/*
 * This file is part of IRCBot
 * Copyright © 2014 Rachel Mant (dx-mon@users.sourceforge.net)
 *
 * IRCBot is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * IRCBot is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __THREADED_QUEUE_H__
#define __THREADED_QUEUE_H__

#include <pthread.h>
#include <queue>

template<class T>
class ThreadedQueue : public std::queue<T>
{
private:
	pthread_mutex_t queueMutex;
	pthread_cond_t queueCond;

public:
	ThreadedQueue()
	{
		pthread_mutexattr_t mutexAttrs;
		pthread_condattr_t condAttrs;

		pthread_mutexattr_init(&mutexAttrs);
		pthread_mutexattr_settype(&mutexAttrs, PTHREAD_MUTEX_ERRORCHECK);
		pthread_mutex_init(&queueMutex, &mutexAttrs);
		pthread_mutexattr_destroy(&mutexAttrs);

		pthread_condattr_init(&condAttrs);
		pthread_condattr_setpshared(&condAttrs, PTHREAD_PROCESS_PRIVATE);
		pthread_cond_init(&queueCond, &condAttrs);
		pthread_condattr_destroy(&condAttrs);
	}

	~ThreadedQueue()
	{
		pthread_cond_destroy(&queueCond);
		pthread_mutex_destroy(&queueMutex);
	}

	void waitItems()
	{
		pthread_mutex_lock(&queueMutex);
		pthread_cond_wait(&queueCond, &queueMutex);
		pthread_mutex_unlock(&queueMutex);
	}

	void signalItems()
	{
		pthread_mutex_lock(&queueMutex);
		pthread_cond_broadcast(&queueCond);
		pthread_mutex_unlock(&queueMutex);
	}

	void push(T item)
	{
		std::queue<T>::push(item);
		signalItems();
	}
};

#endif /*__THREADED_QUEUE_H__*/
