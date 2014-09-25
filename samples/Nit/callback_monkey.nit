# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This sample has been implemented to show you how simple is it to play 
# with native callbacks (C) through an high level with NIT program.

module callback_monkey

in "C header" `{
	#include <stdio.h>
	#include <stdlib.h>

	typedef struct { 
		int id;
		int age;
	} CMonkey;

	typedef struct {
		MonkeyActionCallable toCall;
		Object message;
	} MonkeyAction;
`}

in "C body" `{
	// Method which reproduce a callback answer
	// Please note that a function pointer is only used to reproduce the callback
	void cbMonkey(CMonkey *mkey, void callbackFunc(CMonkey*, MonkeyAction*), MonkeyAction *data)
	{
		sleep(2);
		callbackFunc( mkey, data );
	}

	// Back of background treatment, will be redirected to callback function
	void nit_monkey_callback_func( CMonkey *mkey, MonkeyAction *data )
	{
		// To call a your method, the signature must be written like this :
		// <Interface Name>_<Method>...
		MonkeyActionCallable_wokeUp( data->toCall, mkey, data->message );
	}
`}

# Implementable interface to get callback in defined methods
interface MonkeyActionCallable
	fun wokeUp( sender:Monkey, message: Object) is abstract
end

# Defining my object type Monkey, which is, in a low level, a pointer to a C struct (CMonkey)
extern class Monkey `{ CMonkey * `}
	
	new `{
		CMonkey *monkey = malloc( sizeof(CMonkey) );
		monkey->age = 10;
		monkey->id = 1;
		return monkey;
	`}
	
	# Object method which will get a callback in wokeUp method, defined in MonkeyActionCallable interface
	# Must be defined as Nit/C method because of C call inside
	fun wokeUpAction( toCall: MonkeyActionCallable, message: Object ) is extern import MonkeyActionCallable.wokeUp `{

		// Allocating memory to keep reference of received parameters :
		// - Object receiver
		// - Message 
		MonkeyAction *data = malloc( sizeof(MonkeyAction) );

		// Incrementing reference counter to prevent from releasing
		MonkeyActionCallable_incr_ref( toCall );
		Object_incr_ref( message );
		
		data->toCall = toCall;
		data->message = message;
		
		// Calling method which reproduce a callback by passing :
		// - Receiver
		// - Function pointer to object return method
		// - Datas
		cbMonkey( recv, &nit_monkey_callback_func, data );
	`}
end
