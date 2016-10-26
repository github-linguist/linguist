
/*
 * This file is part of Jkop
 * Copyright (c) 2016 Job and Esther Technologies, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

IFNDEF("target_posix")
{
	public class HTTPServerVirtualHostListener : EventLoopReadListener
	{
		public static HTTPServerVirtualHostListener create(String name, HTTPServer server, String prefix = null) {
			Logger logger;
			if(server != null) {
				logger = server.get_logger();
			}
			Log.error("Virtual hosts are not supported on this platform.", logger);
			return(null);
		}

		public void on_read_ready() {
		}

		public void close() {
		}
	}
}

ELSE {

public class HTTPServerVirtualHostListener : LoggerObject, EventLoopReadListener
{
	class Client : LoggerObject, EventLoopReadListener
	{
		HTTPServer server;
		LocalSocket socket;
		EventLoopEntry ee;

		embed "c" {{{
			#include <unistd.h>
			#include <sys/types.h>
			#include <sys/socket.h>
		}}}

		public static Client create(HTTPServer server, LocalSocket socket) {
			if(server == null || socket == null) {
				return(null);
			}
			var eventloop = server.get_eventloop();
			if(eventloop == null) {
				return(null);
			}
			var v = new Client();
			v.set_logger(server.get_logger());
			v.server = server;
			v.socket = socket;
			v.ee = eventloop.entry_for_object(socket);
			if(v.ee == null) {
				return(null);
			}
			v.ee.set_read_listener(v);
			return(v);
		}

		public void close() {
			if(ee != null) {
				ee.remove();
				ee = null;
			}
			if(socket != null) {
				socket.close();
				socket = null;
			}
			server = null;
		}

		public void on_read_ready() {
			receive_fd();
			close();
		}

		private void receive_fd() {
			if(socket is FileDescriptor == false) {
				return;
			}
			int lsfd = ((FileDescriptor)socket).get_fd();
			int r;
			int newfd = -1;
			embed "c" {{{
				char buf[64];
				struct msghdr msg;
				struct iovec iov[1];
				ssize_t n;
				union {
					struct cmsghdr cm;
					char control[CMSG_SPACE(sizeof(int))];
				} control_un;
				struct cmsghdr* cmptr;
				msg.msg_control = control_un.control;
				msg.msg_controllen = sizeof(control_un.control);
				msg.msg_name = NULL;
				msg.msg_namelen = 0;
				iov[0].iov_base = buf;
				iov[0].iov_len = 64;
				msg.msg_iov = iov;
				msg.msg_iovlen = 1;
				r = recvmsg(lsfd, &msg, 0);
			}}}
			if(r < 0) {
				log_error("FAILED to recvmsg() from the local socket in virtual host receiver.");
				return;
			}
			embed "c" {{{
				if((cmptr = CMSG_FIRSTHDR(&msg)) != NULL && cmptr->cmsg_len == CMSG_LEN(sizeof(int))) {
					if(cmptr->cmsg_level == SOL_SOCKET && cmptr->cmsg_type == SCM_RIGHTS) {
						newfd = *((int*)CMSG_DATA(cmptr));
					}
				}
			}}}
			if(newfd < 0) {
				log_warning("No file descriptor was passed through the socket in virtual host receiver.");
				return;
			}
			var newsock = TCPSocket.create();
			if(newsock == null) {
				embed "c" {{{ close(newfd); }}}
				log_error("FAILED to create a new socket");
				return;
			}
			var fds = newsock as FileDescriptorSocket;
			if(fds == null) {
				embed "c" {{{ close(newfd); }}}
				log_error("TCPSocket is not a FileDescriptorSocket. Cannot set file descriptor.");
				return;
			}
			fds.set_fd(newfd);
			server.on_new_client_socket(newsock);
		}
	}

	HTTPServer server;
	LocalSocket socket;
	EventLoopEntry ee;
	String socketprefix;

	public static HTTPServerVirtualHostListener create(String name, HTTPServer server, String prefix = null) {
		if(server == null) {
			return(null);
		}
		var v = new HTTPServerVirtualHostListener();
		v.server = server;
		v.socketprefix = prefix;
		v.set_logger(server.get_logger());
		if(v.start(name) == false) {
			v = null;
		}
		return(v);
	}

	public bool start(String name) {
		if(String.is_empty(name) || server == null) {
			return(false);
		}
		socket = LocalSocket.create();
		if(socket == null) {
			log_error("Cannot create local socket");
			return(false);
		}
		var pp = socketprefix;
		if(String.is_empty(pp)) {
			pp = "sympathy_vhost_";
		}
		var socketname = "%s%s".printf().add(pp).add(name).to_string();
		if(socket != null) {
			if(socket.listen(socketname) == false) {
				socket = null;
			}
		}
		if(socket == null) {
			log_error("FAILED to listen to local socket `%s'".printf().add(socketname));
			return(false);
		}
		var event_loop = server.get_eventloop();
		if(event_loop == null) {
			log_error("No eventloop");
			return(false);
		}
		ee = event_loop.entry_for_object(socket);
		if(ee == null) {
			log_error("Failed to register socket with eventloop");
			return(false);
		}
		ee.set_read_listener(this);
		log_debug("HTTPServerVirtualHostListener: Listening for virtual host `%s'".printf().add(name));
		return(true);
	}

	public void close() {
		if(ee != null) {
			ee.remove();
			ee = null;
		}
		if(socket != null) {
			socket.close();
			socket = null;
		}
		server = null;
	}

	public void on_read_ready() {
		var ns = socket.accept();
		if(ns != null) {
			Client.create(server, ns);
		}
	}
}

}
