/*
 * Bunnyhop Statistics API - Include file
 * by: shavit
 *
 * This file is part of Bunnyhop Statistics API.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
*/

#if defined _bhopstats_included
	#endinput
#endif
#define _bhopstats_included

#define BHOPSTATS_VERSION "1.2.0"

/**
 * Called when the jump key is pressed.
 *
 * @param client					Client index.
 * @param onground					True if the jump key will do anything for the player when tapped.
 * @noreturn
 */
forward void Bunnyhop_OnJumpPressed(int client, bool onground);

/**
 * Called when the jump key is released.
 *
 * @param client					Client index.
 * @param onground					True if the jump key will do anything for the player when tapped.
 * @noreturn
 */
forward void Bunnyhop_OnJumpReleased(int client, bool onground);

/**
 * Called when the player touches the ground.
 *
 * @param client					Client index.
 * @noreturn
 */
forward void Bunnyhop_OnTouchGround(int client);

/**
 * Called when the player leaves the ground, by either jumping or falling from somewhere.
 * AKA the HookEventless better version of player_jump.
 * The `jumped` variable is true if the ground was left by tapping the jump key, or false if the player fell from somewhere.
 * `ladder` is true if the player left the 'ground' from a ladder.
 *
 * @param client					Client index.
 * @param jumped					Did the client leave the ground by jumping?
 * @param ladder					Did the client leave the ground by leaving a ladder, aka ladderstrafing?
 * @noreturn
 */
forward void Bunnyhop_OnLeaveGround(int client, bool jumped, bool ladder);

/**
 * Retrieves the amount of separate +jump inputs since the player left the ground.
 *
 * @param client					Client index.
 * @return							Amount of +jump inputs since the left the ground, or 0 if the player is on ground.
 */
native int Bunnyhop_GetScrollCount(int client);

/**
 * Checks if the player is on ground, or if the jump key will function as in actually triggering a jump or altering velocity.
 * The result will be true if the player is on a ladder or in water, as jumping will be functional.
 *
 * @param client					Client index.
 * @return							Boolean value of 'is the player on ground?'
 */
native bool Bunnyhop_IsOnGround(int client);

/**
 * Checks if the player is holding his jump key.
 *
 * @param client					Client index.
 * @return							Boolean value of 'is the player holding the jump key?''
 */
native bool Bunnyhop_IsHoldingJump(int client);

/**
 * Gets a percentage of perfectly timed bunnyhops.
 * Resets at player connection or the Bunnyhop_ResetPerfectJumps native for it is called.
 *
 * @param client					Client index.
 * @return							Perfect jump percentage. Results are from 0.0 to 100.0.
 */
native float Bunnyhop_GetPerfectJumps(int client);

/**
 * Resets the perfect jumps percentage of a player back to 0.0.
 *
 * @param client					Client index.
 * @noreturn
 */
native void Bunnyhop_ResetPerfectJumps(int client);

methodmap BunnyhopStats __nullable__
{
	public BunnyhopStats(int client)
	{
		return view_as<BunnyhopStats>(client);
	}

	property int index
	{
		public get()
		{
			return view_as<int>(this);
		}
	}

	property int ScrollCount
	{
		public get()
		{
			return Bunnyhop_GetScrollCount(this.index);
		}
	}

	property bool OnGround
    {
		public get()
		{
			return Bunnyhop_IsOnGround(this.index);
		}
    }

	property bool HoldingJump
    {
		public get()
		{
			return Bunnyhop_IsHoldingJump(this.index);
		}
    }

	property float PerfectJumps
    {
		public get()
		{
			return Bunnyhop_GetPerfectJumps(this.index);
		}
    }

	public void ResetPrefects()
	{
		Bunnyhop_ResetPerfectJumps(this.index);
	}

	public static int GetScrollCount(int client)
    {
		return Bunnyhop_GetScrollCount(client);
    }

	public static bool IsOnGround(int client)
    {
		return Bunnyhop_IsOnGround(client);
    }

	public static bool IsHoldingJump(int client)
    {
		return Bunnyhop_IsHoldingJump(client);
    }

	public static float GetPerfectJumps(int client)
    {
		return Bunnyhop_GetPerfectJumps(client);
    }

	public static float ResetPrefectJumps(int client)
    {
		return Bunnyhop_ResetPerfectJumps(client);
    }
}

public SharedPlugin __pl_bhopstats =
{
	name = "bhopstats",
	file = "bhopstats.smx",
#if defined REQUIRE_PLUGIN
	required = 1
#else
	required = 0
#endif
};

#if !defined REQUIRE_PLUGIN
public void __pl_bhopstats_SetNTVOptional()
{
	MarkNativeAsOptional("Bunnyhop_GetScrollCount");
	MarkNativeAsOptional("Bunnyhop_IsOnGround");
	MarkNativeAsOptional("Bunnyhop_IsHoldingJump");
	MarkNativeAsOptional("Bunnyhop_GetPerfectJumps");
	MarkNativeAsOptional("Bunnyhop_ResetPerfectJumps");
}
#endif
