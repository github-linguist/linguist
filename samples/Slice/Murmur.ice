// Copyright 2005-2018 The Mumble Developers. All rights reserved.
// Use of this source code is governed by a BSD-style license
// that can be found in the LICENSE file at the root of the
// Mumble source tree or at <https://www.mumble.info/LICENSE>.

/**
 *
 * Information and control of the murmur server. Each server has
 * one {@link Meta} interface that controls global information, and
 * each virtual server has a {@link Server} interface.
 *
 **/

#include <Ice/SliceChecksumDict.ice>

module Murmur
{

	/** A network address in IPv6 format.
	 **/
	["python:seq:tuple"] sequence<byte> NetAddress;

	/** A connected user.
	 **/
	struct User {
		/** Session ID. This identifies the connection to the server. */
		int session;
		/** User ID. -1 if the user is anonymous. */
		int userid;
		/** Is user muted by the server? */
		bool mute;
		/** Is user deafened by the server? If true, this implies mute. */
		bool deaf;
		/** Is the user suppressed by the server? This means the user is not muted, but does not have speech privileges in the current channel. */
		bool suppress;
		/** Is the user a priority speaker? */
		bool prioritySpeaker;
		/** Is the user self-muted? */
		bool selfMute;
		/** Is the user self-deafened? If true, this implies mute. */
		bool selfDeaf;
		/** Is the User recording? (This flag is read-only and cannot be changed using setState().) **/
		bool recording;
		/** Channel ID the user is in. Matches {@link Channel.id}. */
		int channel;
		/** The name of the user. */
		string name;
		/** Seconds user has been online. */
		int onlinesecs;
		/** Average transmission rate in bytes per second over the last few seconds. */
		int bytespersec;
		/** Client version. Major version in upper 16 bits, followed by 8 bits of minor version and 8 bits of patchlevel. Version 1.2.3 = 0x010203. */
		int version;
		/** Client release. For official releases, this equals the version. For snapshots and git compiles, this will be something else. */
		string release;
		/** Client OS. */
		string os;
		/** Client OS Version. */
		string osversion;
		/** Plugin Identity. This will be the user's unique ID inside the current game. */
		string identity;
		/**
		   Base64-encoded Plugin context. This is a binary blob identifying the game and team the user is on.

		   The used Base64 alphabet is the one specified in RFC 2045.

		   Before Mumble 1.3.0, this string was not Base64-encoded. This could cause problems for some Ice
		   implementations, such as the .NET implementation.

		   If you need the exact string that is used by Mumble, you can get it by Base64-decoding this string.

		   If you simply need to detect whether two users are in the same game world, string comparisons will
		   continue to work as before.
		 */
		string context;
		/** User comment. Shown as tooltip for this user. */
		string comment;
		/** Client address. */
		NetAddress address;
		/** TCP only. True until UDP connectivity is established. */
		bool tcponly;
		/** Idle time. This is how many seconds it is since the user last spoke. Other activity is not counted. */
		int idlesecs;
		/** UDP Ping Average. This is the average ping for the user via UDP over the duration of the connection. */
		float udpPing;
		/** TCP Ping Average. This is the average ping for the user via TCP over the duration of the connection. */
		float tcpPing;
	};

	sequence<int> IntList;

	/** A text message between users.
	 **/
	struct TextMessage {
		/** Sessions (connected users) who were sent this message. */
		IntList sessions;
		/** Channels who were sent this message. */
		IntList channels;
		/** Trees of channels who were sent this message. */
		IntList trees;
		/** The contents of the message. */
		string text;
	};

	/** A channel.
	 **/
	struct Channel {
		/** Channel ID. This is unique per channel, and the root channel is always id 0. */
		int id;
		/** Name of the channel. There can not be two channels with the same parent that has the same name. */
		string name;
		/** ID of parent channel, or -1 if this is the root channel. */
		int parent;
		/** List of id of linked channels. */
		IntList links;
		/** Description of channel. Shown as tooltip for this channel. */
		string description;
		/** Channel is temporary, and will be removed when the last user leaves it. */
		bool temporary;
		/** Position of the channel which is used in Client for sorting. */
		int position;
	};

	/** A group. Groups are defined per channel, and can inherit members from parent channels.
	 **/
	struct Group {
		/** Group name */
		string name;
		/** Is this group inherited from a parent channel? Read-only. */
		bool inherited;
		/** Does this group inherit members from parent channels? */
		bool inherit;
		/** Can subchannels inherit members from this group? */
		bool inheritable;
		/** List of users to add to the group. */
		IntList add;
		/** List of inherited users to remove from the group. */
		IntList remove;
		/** Current members of the group, including inherited members. Read-only. */
		IntList members;
	};

	/** Write access to channel control. Implies all other permissions (except Speak). */
	const int PermissionWrite = 0x01;
	/** Traverse channel. Without this, a client cannot reach subchannels, no matter which privileges he has there. */
	const int PermissionTraverse = 0x02;
	/** Enter channel. */
	const int PermissionEnter = 0x04;
	/** Speak in channel. */
	const int PermissionSpeak = 0x08;
	/** Whisper to channel. This is different from Speak, so you can set up different permissions. */
	const int PermissionWhisper = 0x100;
	/** Mute and deafen other users in this channel. */
	const int PermissionMuteDeafen = 0x10;
	/** Move users from channel. You need this permission in both the source and destination channel to move another user. */
	const int PermissionMove = 0x20;
	/** Make new channel as a subchannel of this channel. */
	const int PermissionMakeChannel = 0x40;
	/** Make new temporary channel as a subchannel of this channel. */
	const int PermissionMakeTempChannel = 0x400;
	/** Link this channel. You need this permission in both the source and destination channel to link channels, or in either channel to unlink them. */
	const int PermissionLinkChannel = 0x80;
	/** Send text message to channel. */
	const int PermissionTextMessage = 0x200;
	/** Kick user from server. Only valid on root channel. */
	const int PermissionKick = 0x10000;
	/** Ban user from server. Only valid on root channel. */
	const int PermissionBan = 0x20000;
	/** Register and unregister users. Only valid on root channel. */
	const int PermissionRegister = 0x40000;
	/** Register and unregister users. Only valid on root channel. */
	const int PermissionRegisterSelf = 0x80000;


	/** Access Control List for a channel. ACLs are defined per channel, and can be inherited from parent channels.
	 **/
	struct ACL {
		/** Does the ACL apply to this channel? */
		bool applyHere;
		/** Does the ACL apply to subchannels? */
		bool applySubs;
		/** Is this ACL inherited from a parent channel? Read-only. */
		bool inherited;
		/** ID of user this ACL applies to. -1 if using a group name. */
		int userid;
		/** Group this ACL applies to. Blank if using userid. */
		string group;
		/** Binary mask of privileges to allow. */
		int allow;
		/** Binary mask of privileges to deny. */
		int deny;
	};

	/** A single ip mask for a ban.
	 **/
	struct Ban {
		/** Address to ban. */
		NetAddress address;
		/** Number of bits in ban to apply. */
		int bits;
		/** Username associated with ban. */
		string name;
		/** Hash of banned user. */
		string hash;
		/** Reason for ban. */
		string reason;
		/** Date ban was applied in unix time format. */
		int start;
		/** Duration of ban. */
		int duration;
	};

	/** A entry in the log.
	 **/
	struct LogEntry {
		/** Timestamp in UNIX time_t */
		int timestamp;
		/** The log message. */
		string txt;
	};

	class Tree;
	sequence<Tree> TreeList;

	enum ChannelInfo { ChannelDescription, ChannelPosition };
	enum UserInfo { UserName, UserEmail, UserComment, UserHash, UserPassword, UserLastActive };

	dictionary<int, User> UserMap;
	dictionary<int, Channel> ChannelMap;
	sequence<Channel> ChannelList;
	sequence<User> UserList;
	sequence<Group> GroupList;
	sequence<ACL> ACLList;
	sequence<LogEntry> LogList;
	sequence<Ban> BanList;
	sequence<int> IdList;
	sequence<string> NameList;
	dictionary<int, string> NameMap;
	dictionary<string, int> IdMap;
	sequence<byte> Texture;
	dictionary<string, string> ConfigMap;
	sequence<string> GroupNameList;
	sequence<byte> CertificateDer;
	sequence<CertificateDer> CertificateList;

	/** User information map.
	 * Older versions of ice-php can't handle enums as keys. If you are using one of these, replace 'UserInfo' with 'byte'.
	 */

	dictionary<UserInfo, string> UserInfoMap;

	/** User and subchannel state. Read-only.
	 **/
	class Tree {
		/** Channel definition of current channel. */
		Channel c;
		/** List of subchannels. */
		TreeList children;
		/** Users in this channel. */
		UserList users;
	};

	exception MurmurException {};
	/** This is thrown when you specify an invalid session. This may happen if the user has disconnected since your last call to {@link Server.getUsers}. See {@link User.session} */
	exception InvalidSessionException extends MurmurException {};
	/** This is thrown when you specify an invalid channel id. This may happen if the channel was removed by another provess. It can also be thrown if you try to add an invalid channel. */
	exception InvalidChannelException extends MurmurException {};
	/** This is thrown when you try to do an operation on a server that does not exist. This may happen if someone has removed the server. */
	exception InvalidServerException extends MurmurException {};
	/** This happens if you try to fetch user or channel state on a stopped server, if you try to stop an already stopped server or start an already started server. */
	exception ServerBootedException extends MurmurException {};
	/** This is thrown if {@link Server.start} fails, and should generally be the cause for some concern. */
	exception ServerFailureException extends MurmurException {};
	/** This is thrown when you specify an invalid userid. */
	exception InvalidUserException extends MurmurException {};
	/** This is thrown when you try to set an invalid texture. */
	exception InvalidTextureException extends MurmurException {};
	/** This is thrown when you supply an invalid callback. */
	exception InvalidCallbackException extends MurmurException {};
	/**  This is thrown when you supply the wrong secret in the calling context. */
	exception InvalidSecretException extends MurmurException {};
	/** This is thrown when the channel operation would excede the channel nesting limit */
	exception NestingLimitException extends MurmurException {};
	/**  This is thrown when you ask the server to disclose something that should be secret. */
	exception WriteOnlyException extends MurmurException {};
	/** This is thrown when invalid input data was specified. */
	exception InvalidInputDataException extends MurmurException {};

	/** Callback interface for servers. You can supply an implementation of this to receive notification
	 *  messages from the server.
	 *  If an added callback ever throws an exception or goes away, it will be automatically removed.
	 *  Please note that all callbacks are done asynchronously; murmur does not wait for the callback to
	 *  complete before continuing processing.
	 *  Note that callbacks are removed when a server is stopped, so you should have a callback for
	 *  {@link MetaCallback.started} which calls {@link Server.addCallback}.
	 *  @see MetaCallback
	 *  @see Server.addCallback
	 */
	interface ServerCallback {
		/** Called when a user connects to the server. 
		 *  @param state State of connected user.
		 */
		idempotent void userConnected(User state);
		/** Called when a user disconnects from the server. The user has already been removed, so you can no longer use methods like {@link Server.getState}
		 *  to retrieve the user's state.
		 *  @param state State of disconnected user.
		 */
		idempotent void userDisconnected(User state);
		/** Called when a user state changes. This is called if the user moves, is renamed, is muted, deafened etc.
		 *  @param state New state of user.
		 */
		idempotent void userStateChanged(User state);
		/** Called when user writes a text message
		 *  @param state the User sending the message
		 *  @param message the TextMessage the user has sent
		 */
		idempotent void userTextMessage(User state, TextMessage message);
		/** Called when a new channel is created. 
		 *  @param state State of new channel.
		 */
		idempotent void channelCreated(Channel state);
		/** Called when a channel is removed. The channel has already been removed, you can no longer use methods like {@link Server.getChannelState}
		 *  @param state State of removed channel.
		 */
		idempotent void channelRemoved(Channel state);
		/** Called when a new channel state changes. This is called if the channel is moved, renamed or if new links are added.
		 *  @param state New state of channel.
		 */
		idempotent void channelStateChanged(Channel state);
	};

	/** Context for actions in the Server menu. */
	const int ContextServer = 0x01;
	/** Context for actions in the Channel menu. */
	const int ContextChannel = 0x02;
	/** Context for actions in the User menu. */
	const int ContextUser = 0x04;

	/** Callback interface for context actions. You need to supply one of these for {@link Server.addContext}. 
	 *  If an added callback ever throws an exception or goes away, it will be automatically removed.
	 *  Please note that all callbacks are done asynchronously; murmur does not wait for the callback to
	 *  complete before continuing processing.
	 */
	interface ServerContextCallback {
		/** Called when a context action is performed.
		 *  @param action Action to be performed.
		 *  @param usr User which initiated the action.
		 *  @param session If nonzero, session of target user.
		 *  @param channelid If not -1, id of target channel.
		 */
		idempotent void contextAction(string action, User usr, int session, int channelid);
	};

	/** Callback interface for server authentication. You need to supply one of these for {@link Server.setAuthenticator}.
	 *  If an added callback ever throws an exception or goes away, it will be automatically removed.
	 *  Please note that unlike {@link ServerCallback} and {@link ServerContextCallback}, these methods are called
	 *  synchronously. If the response lags, the entire murmur server will lag.
	 *  Also note that, as the method calls are synchronous, making a call to {@link Server} or {@link Meta} will
	 *  deadlock the server.
	 */
	interface ServerAuthenticator {
		/** Called to authenticate a user. If you do not know the username in question, always return -2 from this
		 *  method to fall through to normal database authentication.
		 *  Note that if authentication succeeds, murmur will create a record of the user in it's database, reserving
		 *  the username and id so it cannot be used for normal database authentication.
		 *  The data in the certificate (name, email addresses etc), as well as the list of signing certificates,
		 *  should only be trusted if certstrong is true.
		 *
		 *  Internally, Murmur treats usernames as case-insensitive. It is recommended
		 *  that authenticators do the same. Murmur checks if a username is in use when
		 *  a user connects. If the connecting user is registered, the other username is
		 *  kicked. If the connecting user is not registered, the connecting user is not
		 *  allowed to join the server.
		 *
		 *  @param name Username to authenticate.
		 *  @param pw Password to authenticate with.
		 *  @param certificates List of der encoded certificates the user connected with.
		 *  @param certhash Hash of user certificate, as used by murmur internally when matching.
		 *  @param certstrong True if certificate was valid and signed by a trusted CA.
		 *  @param newname Set this to change the username from the supplied one.
		 *  @param groups List of groups on the root channel that the user will be added to for the duration of the connection.
		 *  @return UserID of authenticated user, -1 for authentication failures, -2 for unknown user (fallthrough),
		 *          -3 for authentication failures where the data could (temporarily) not be verified.
		 */
		idempotent int authenticate(string name, string pw, CertificateList certificates, string certhash, bool certstrong, out string newname, out GroupNameList groups);

		/** Fetch information about a user. This is used to retrieve information like email address, keyhash etc. If you
		 *  want murmur to take care of this information itself, simply return false to fall through.
		 *  @param id User id.
		 *  @param info Information about user. This needs to include at least "name".
		 *  @return true if information is present, false to fall through.
		 */
		idempotent bool getInfo(int id, out UserInfoMap info);
	
		/** Map a name to a user id.
		 *  @param name Username to map.
		 *  @return User id or -2 for unknown name.
		 */
		idempotent int nameToId(string name);

		/** Map a user id to a username.
		 *  @param id User id to map.
		 *  @return Name of user or empty string for unknown id.
		 */
		idempotent string idToName(int id);

		/** Map a user to a custom Texture.
		 *  @param id User id to map.
		 *  @return User texture or an empty texture for unknwon users or users without textures.
		 */
		idempotent Texture idToTexture(int id);
	};

	/** Callback interface for server authentication and registration. This allows you to support both authentication
	 *  and account updating.
	 *  You do not need to implement this if all you want is authentication, you only need this if other scripts
	 *  connected to the same server calls e.g. {@link Server.setTexture}.
	 *  Almost all of these methods support fall through, meaning murmur should continue the operation against its
	 *  own database.
	 */
	interface ServerUpdatingAuthenticator extends ServerAuthenticator {
		/** Register a new user.
		 *  @param info Information about user to register.
		 *  @return User id of new user, -1 for registration failure, or -2 to fall through.
		 */
		int registerUser(UserInfoMap info);

		/** Unregister a user.
		 *  @param id Userid to unregister.
		 *  @return 1 for successfull unregistration, 0 for unsuccessfull unregistration, -1 to fall through.
		 */
		int unregisterUser(int id);

		/** Get a list of registered users matching filter.
		 *  @param filter Substring usernames must contain. If empty, return all registered users.
		 *  @return List of matching registered users.
		 */
		idempotent NameMap getRegisteredUsers(string filter);

		/** Set additional information for user registration.
		 *  @param id Userid of registered user.
		 *  @param info Information to set about user. This should be merged with existing information.
		 *  @return 1 for successfull update, 0 for unsuccessfull update, -1 to fall through.
		 */
		idempotent int setInfo(int id, UserInfoMap info);

		/** Set texture (now called avatar) of user registration.
		 *  @param id registrationId of registered user.
		 *  @param tex New texture.
		 *  @return 1 for successfull update, 0 for unsuccessfull update, -1 to fall through.
		 */
		idempotent int setTexture(int id, Texture tex);
	};

	/** Per-server interface. This includes all methods for configuring and altering
	 * the state of a single virtual server. You can retrieve a pointer to this interface
	 * from one of the methods in {@link Meta}.
	 **/
	["amd"] interface Server {
		/** Shows if the server currently running (accepting users).
		 *
		 * @return Run-state of server.
		 */
		idempotent bool isRunning() throws InvalidSecretException;

		/** Start server. */
		void start() throws ServerBootedException, ServerFailureException, InvalidSecretException;

		/** Stop server.
		 * Note: Server will be restarted on Murmur restart unless explicitly disabled
		 *       with setConf("boot", false)
		 */
		void stop() throws ServerBootedException, InvalidSecretException;

		/** Delete server and all it's configuration. */
		void delete() throws ServerBootedException, InvalidSecretException;

		/** Fetch the server id.
		 *
		 * @return Unique server id.
		 */
		idempotent int id() throws InvalidSecretException;

		/** Add a callback. The callback will receive notifications about changes to users and channels.
		 *
		 * @param cb Callback interface which will receive notifications.
		 * @see removeCallback
		 */
		void addCallback(ServerCallback *cb) throws ServerBootedException, InvalidCallbackException, InvalidSecretException;

		/** Remove a callback.
		 *
		 * @param cb Callback interface to be removed.
		 * @see addCallback
		 */
		void removeCallback(ServerCallback *cb) throws ServerBootedException, InvalidCallbackException, InvalidSecretException;

		/** Set external authenticator. If set, all authentications from clients are forwarded to this
		 *  proxy.
		 *
		 * @param auth Authenticator object to perform subsequent authentications.
		 */
		void setAuthenticator(ServerAuthenticator *auth) throws ServerBootedException, InvalidCallbackException, InvalidSecretException;

		/** Retrieve configuration item.
		 * @param key Configuration key.
		 * @return Configuration value. If this is empty, see {@link Meta.getDefaultConf}
		 */
		idempotent string getConf(string key) throws InvalidSecretException, WriteOnlyException;

		/** Retrieve all configuration items.
		 * @return All configured values. If a value isn't set here, the value from {@link Meta.getDefaultConf} is used.
		 */
		idempotent ConfigMap getAllConf() throws InvalidSecretException;

		/** Set a configuration item.
		 * @param key Configuration key.
		 * @param value Configuration value.
		 */
		idempotent void setConf(string key, string value) throws InvalidSecretException;

		/** Set superuser password. This is just a convenience for using {@link updateRegistration} on user id 0.
		 * @param pw Password.
		 */
		idempotent void setSuperuserPassword(string pw) throws InvalidSecretException;

		/** Fetch log entries.
		 * @param first Lowest numbered entry to fetch. 0 is the most recent item.
		 * @param last Last entry to fetch.
		 * @return List of log entries.
		 */
		idempotent LogList getLog(int first, int last) throws InvalidSecretException;

		/** Fetch length of log
		 * @return Number of entries in log
		 */
		idempotent int getLogLen() throws InvalidSecretException;

		/** Fetch all users. This returns all currently connected users on the server.
		 * @return List of connected users.
		 * @see getState
		 */
		idempotent UserMap getUsers() throws ServerBootedException, InvalidSecretException;

		/** Fetch all channels. This returns all defined channels on the server. The root channel is always channel 0.
		 * @return List of defined channels.
		 * @see getChannelState
		 */
		idempotent ChannelMap getChannels() throws ServerBootedException, InvalidSecretException;

		/** Fetch certificate of user. This returns the complete certificate chain of a user.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @return Certificate list of user.
		 */
		idempotent CertificateList getCertificateList(int session) throws ServerBootedException, InvalidSessionException, InvalidSecretException;

		/** Fetch all channels and connected users as a tree. This retrieves an easy-to-use representation of the server
		 *  as a tree. This is primarily used for viewing the state of the server on a webpage.
		 * @return Recursive tree of all channels and connected users.
		 */
		idempotent Tree getTree() throws ServerBootedException, InvalidSecretException;

		/** Fetch all current IP bans on the server.
		 * @return List of bans.
		 */
		idempotent BanList getBans() throws ServerBootedException, InvalidSecretException;

		/** Set all current IP bans on the server. This will replace any bans already present, so if you want to add a ban, be sure to call {@link getBans} and then
		 *  append to the returned list before calling this method.
		 * @param bans List of bans.
		 */
		idempotent void setBans(BanList bans) throws ServerBootedException, InvalidSecretException;

		/** Kick a user. The user is not banned, and is free to rejoin the server.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param reason Text message to show when user is kicked.
		 */
		void kickUser(int session, string reason) throws ServerBootedException, InvalidSessionException, InvalidSecretException;

		/** Get state of a single connected user. 
		 * @param session Connection ID of user. See {@link User.session}.
		 * @return State of connected user.
		 * @see setState
		 * @see getUsers
		 */
		idempotent User getState(int session) throws ServerBootedException, InvalidSessionException, InvalidSecretException;

		/** Set user state. You can use this to move, mute and deafen users.
		 * @param state User state to set.
		 * @see getState
		 */
		idempotent void setState(User state) throws ServerBootedException, InvalidSessionException, InvalidChannelException, InvalidSecretException;

		/** Send text message to a single user.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param text Message to send.
		 * @see sendMessageChannel
		 */
		void sendMessage(int session, string text) throws ServerBootedException, InvalidSessionException, InvalidSecretException;

		/** Check if user is permitted to perform action.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param channelid ID of Channel. See {@link Channel.id}.
		 * @param perm Permission bits to check.
		 * @return true if any of the permissions in perm were set for the user.
		 */
		bool hasPermission(int session, int channelid, int perm) throws ServerBootedException, InvalidSessionException, InvalidChannelException, InvalidSecretException;
		
		/** Return users effective permissions
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param channelid ID of Channel. See {@link Channel.id}.
		 * @return bitfield of allowed actions
		 */
		idempotent int effectivePermissions(int session, int channelid) throws ServerBootedException, InvalidSessionException, InvalidChannelException, InvalidSecretException;

		/** Add a context callback. This is done per user, and will add a context menu action for the user.
		 *
		 * @param session Session of user which should receive context entry.
		 * @param action Action string, a unique name to associate with the action.
		 * @param text Name of action shown to user.
		 * @param cb Callback interface which will receive notifications.
		 * @param ctx Context this should be used in. Needs to be one or a combination of {@link ContextServer}, {@link ContextChannel} and {@link ContextUser}.
		 * @see removeContextCallback
		 */
		void addContextCallback(int session, string action, string text, ServerContextCallback *cb, int ctx) throws ServerBootedException, InvalidCallbackException, InvalidSecretException;

		/** Remove a callback.
		 *
		 * @param cb Callback interface to be removed. This callback will be removed from all from all users.
		 * @see addContextCallback
		 */
		void removeContextCallback(ServerContextCallback *cb) throws ServerBootedException, InvalidCallbackException, InvalidSecretException;
		
		/** Get state of single channel.
		 * @param channelid ID of Channel. See {@link Channel.id}.
		 * @return State of channel.
		 * @see setChannelState
		 * @see getChannels
		 */
		idempotent Channel getChannelState(int channelid) throws ServerBootedException, InvalidChannelException, InvalidSecretException;

		/** Set state of a single channel. You can use this to move or relink channels.
		 * @param state Channel state to set.
		 * @see getChannelState
		 */
		idempotent void setChannelState(Channel state) throws ServerBootedException, InvalidChannelException, InvalidSecretException, NestingLimitException;

		/** Remove a channel and all its subchannels.
		 * @param channelid ID of Channel. See {@link Channel.id}.
		 */
		void removeChannel(int channelid) throws ServerBootedException, InvalidChannelException, InvalidSecretException;

		/** Add a new channel.
		 * @param name Name of new channel.
		 * @param parent Channel ID of parent channel. See {@link Channel.id}.
		 * @return ID of newly created channel.
		 */
		int addChannel(string name, int parent) throws ServerBootedException, InvalidChannelException, InvalidSecretException, NestingLimitException;

		/** Send text message to channel or a tree of channels.
		 * @param channelid Channel ID of channel to send to. See {@link Channel.id}.
		 * @param tree If true, the message will be sent to the channel and all its subchannels.
		 * @param text Message to send.
		 * @see sendMessage
		 */
		void sendMessageChannel(int channelid, bool tree, string text) throws ServerBootedException, InvalidChannelException, InvalidSecretException;

		/** Retrieve ACLs and Groups on a channel.
		 * @param channelid Channel ID of channel to fetch from. See {@link Channel.id}.
		 * @param acls List of ACLs on the channel. This will include inherited ACLs.
		 * @param groups List of groups on the channel. This will include inherited groups.
		 * @param inherit Does this channel inherit ACLs from the parent channel?
		 */
		idempotent void getACL(int channelid, out ACLList acls, out GroupList groups, out bool inherit) throws ServerBootedException, InvalidChannelException, InvalidSecretException;

		/** Set ACLs and Groups on a channel. Note that this will replace all existing ACLs and groups on the channel.
		 * @param channelid Channel ID of channel to fetch from. See {@link Channel.id}.
		 * @param acls List of ACLs on the channel.
		 * @param groups List of groups on the channel.
		 * @param inherit Should this channel inherit ACLs from the parent channel?
		 */
		idempotent void setACL(int channelid, ACLList acls, GroupList groups, bool inherit) throws ServerBootedException, InvalidChannelException, InvalidSecretException;

		/** Temporarily add a user to a group on a channel. This state is not saved, and is intended for temporary memberships.
		 * @param channelid Channel ID of channel to add to. See {@link Channel.id}.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param group Group name to add to.
		 */
		idempotent void addUserToGroup(int channelid, int session, string group) throws ServerBootedException, InvalidChannelException, InvalidSessionException, InvalidSecretException;

		/** Remove a user from a temporary group membership on a channel. This state is not saved, and is intended for temporary memberships.
		 * @param channelid Channel ID of channel to add to. See {@link Channel.id}.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param group Group name to remove from.
		 */
		idempotent void removeUserFromGroup(int channelid, int session, string group) throws ServerBootedException, InvalidChannelException, InvalidSessionException, InvalidSecretException;

		/** Redirect whisper targets for user. If set, whenever a user tries to whisper to group "source", the whisper will be redirected to group "target".
		 * To remove a redirect pass an empty target string. This is intended for context groups.
		 * @param session Connection ID of user. See {@link User.session}.
		 * @param source Group name to redirect from.
		 * @param target Group name to redirect to.
		 */
		idempotent void redirectWhisperGroup(int session, string source, string target) throws ServerBootedException, InvalidSessionException, InvalidSecretException;

		/** Map a list of {@link User.userid} to a matching name.
		 * @param List of ids.
		 * @return Matching list of names, with an empty string representing invalid or unknown ids.
		 */
		idempotent NameMap getUserNames(IdList ids) throws ServerBootedException, InvalidSecretException;

		/** Map a list of user names to a matching id.
		 * @param List of names.
		 * @reuturn List of matching ids, with -1 representing invalid or unknown user names.
		 */
		idempotent IdMap getUserIds(NameList names) throws ServerBootedException, InvalidSecretException;

		/** Register a new user.
		 * @param info Information about new user. Must include at least "name".
		 * @return The ID of the user. See {@link RegisteredUser.userid}.
		 */
		int registerUser(UserInfoMap info) throws ServerBootedException, InvalidUserException, InvalidSecretException;

		/** Remove a user registration.
		 * @param userid ID of registered user. See {@link RegisteredUser.userid}.
		 */
		void unregisterUser(int userid) throws ServerBootedException, InvalidUserException, InvalidSecretException;

		/** Update the registration for a user. You can use this to set the email or password of a user,
		 * and can also use it to change the user's name.
		 * @param registration Updated registration record.
		 */
		idempotent void updateRegistration(int userid, UserInfoMap info) throws ServerBootedException, InvalidUserException, InvalidSecretException;

		/** Fetch registration for a single user.
		 * @param userid ID of registered user. See {@link RegisteredUser.userid}.
		 * @return Registration record.
		 */
		idempotent UserInfoMap getRegistration(int userid) throws ServerBootedException, InvalidUserException, InvalidSecretException;

		/** Fetch a group of registered users.
		 * @param filter Substring of user name. If blank, will retrieve all registered users.
		 * @return List of registration records.
		 */
		idempotent NameMap getRegisteredUsers(string filter) throws ServerBootedException, InvalidSecretException;

		/** Verify the password of a user. You can use this to verify a user's credentials.
		 * @param name User name. See {@link RegisteredUser.name}.
		 * @param pw User password.
		 * @return User ID of registered user (See {@link RegisteredUser.userid}), -1 for failed authentication or -2 for unknown usernames.
		 */
		idempotent int verifyPassword(string name, string pw) throws ServerBootedException, InvalidSecretException;

		/** Fetch user texture. Textures are stored as zlib compress()ed 600x60 32-bit BGRA data.
		 * @param userid ID of registered user. See {@link RegisteredUser.userid}.
		 * @return Custom texture associated with user or an empty texture.
		 */
		idempotent Texture getTexture(int userid) throws ServerBootedException, InvalidUserException, InvalidSecretException;

		/** Set a user texture (now called avatar).
		 * @param userid ID of registered user. See {@link RegisteredUser.userid}.
		 * @param tex Texture (as a Byte-Array) to set for the user, or an empty texture to remove the existing texture.
		 */
		idempotent void setTexture(int userid, Texture tex) throws ServerBootedException, InvalidUserException, InvalidTextureException, InvalidSecretException;

		/** Get virtual server uptime.
		 * @return Uptime of the virtual server in seconds
		 */
		idempotent int getUptime() throws ServerBootedException, InvalidSecretException;

		/**
		 * Update the server's certificate information.
		 *
		 * Reconfigure the running server's TLS socket with the given
		 * certificate and private key.
		 *
		 * The certificate and and private key must be PEM formatted.
		 *
		 * New clients will see the new certificate.
		 * Existing clients will continue to see the certificate the server
		 * was using when they connected to it.
		 *
		 * This method throws InvalidInputDataException if any of the
		 * following errors happen:
		 *  - Unable to decode the PEM certificate and/or private key.
		 *  - Unable to decrypt the private key with the given passphrase.
		 *  - The certificate and/or private key do not contain RSA keys.
		 *  - The certificate is not usable with the given private key.
		 */
		 idempotent void updateCertificate(string certificate, string privateKey, string passphrase) throws ServerBootedException, InvalidSecretException, InvalidInputDataException;
	};

	/** Callback interface for Meta. You can supply an implementation of this to receive notifications
	 *  when servers are stopped or started.
	 *  If an added callback ever throws an exception or goes away, it will be automatically removed.
	 *  Please note that all callbacks are done asynchronously; murmur does not wait for the callback to
	 *  complete before continuing processing.
	 *  @see ServerCallback
	 *  @see Meta.addCallback
	 */
	interface MetaCallback {
		/** Called when a server is started. The server is up and running when this event is sent, so all methods that 
		 *  need a running server will work.
		 *  @param srv Interface for started server.
		 */
		void started(Server *srv);

		/** Called when a server is stopped. The server is already stopped when this event is sent, so no methods that
		 *  need a running server will work.
		 *  @param srv Interface for started server.
		 */
		void stopped(Server *srv);
	};

	sequence<Server *> ServerList;

	/** This is the meta interface. It is primarily used for retrieving the {@link Server} interfaces for each individual server.
	 **/
	["amd"] interface Meta {
		/** Fetch interface to specific server.
		 * @param id Server ID. See {@link Server.getId}.
		 * @return Interface for specified server, or a null proxy if id is invalid.
		 */
		idempotent Server *getServer(int id) throws InvalidSecretException;

		/** Create a new server. Call {@link Server.getId} on the returned interface to find it's ID.
		 * @return Interface for new server.
		 */
		Server *newServer() throws InvalidSecretException;

		/** Fetch list of all currently running servers.
		 * @return List of interfaces for running servers.
		 */
		idempotent ServerList getBootedServers() throws InvalidSecretException;

		/** Fetch list of all defined servers.
		 * @return List of interfaces for all servers.
		 */
		idempotent ServerList getAllServers() throws InvalidSecretException;

		/** Fetch default configuraion. This returns the configuration items that were set in the configuration file, or
		 * the built-in default. The individual servers will use these values unless they have been overridden in the
		 * server specific configuration. The only special case is the port, which defaults to the value defined here +
		 * the servers ID - 1 (so that virtual server #1 uses the defined port, server #2 uses port+1 etc).
		 * @return Default configuration of the servers.
		 */
		idempotent ConfigMap getDefaultConf() throws InvalidSecretException;

		/** Fetch version of Murmur. 
		 * @param major Major version.
		 * @param minor Minor version.
		 * @param patch Patchlevel.
		 * @param text Textual representation of version. Note that this may not match the {@link major}, {@link minor} and {@link patch} levels, as it
		 *   may be simply the compile date or the SVN revision. This is usually the text you want to present to users.
		 */
		idempotent void getVersion(out int major, out int minor, out int patch, out string text);

		/** Add a callback. The callback will receive notifications when servers are started or stopped.
		 *
		 * @param cb Callback interface which will receive notifications.
		 */
		void addCallback(MetaCallback *cb) throws InvalidCallbackException, InvalidSecretException;

		/** Remove a callback.
		 *
		 * @param cb Callback interface to be removed.
		 */
		void removeCallback(MetaCallback *cb) throws InvalidCallbackException, InvalidSecretException;
		
		/** Get murmur uptime.
		 * @return Uptime of murmur in seconds
		 */
		idempotent int getUptime();

		/** Get slice file.
		 * @return Contents of the slice file server compiled with.
		 */
		idempotent string getSlice();

		/** Returns a checksum dict for the slice file.
		 * @return Checksum dict
		 */
		idempotent Ice::SliceChecksumDict getSliceChecksums();
	};
};
