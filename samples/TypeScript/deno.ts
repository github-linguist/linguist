#!/usr/bin/env -S deno run --allow-env

/**
 * @author oopsio
 * @copyright (c) 2026-present oopsio
 * @license MIT
 */

/*
  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
 * Represents a user in the system.
 */
class User {
  // Private fields (encapsulation)
  private id: number;
  private username: string;
  private email: string;

  /**
   * Creates a new User instance.
   * @param id - Unique identifier for the user.
   * @param username - Chosen display name.
   * @param email - User's contact email.
   */
  constructor(id: number, username: string, email: string) {
    this.id = id;
    this.username = username;
    this.email = email;
  }

  /**
   * Displays the user's profile information.
   */
  public displayProfile(): void {
    console.log(`[User] ID: ${this.id} | Name: ${this.username} | Email: ${this.email}`);
  }

  /**
   * Updates the user's email address.
   * @param newEmail - The new email address.
   */
  public updateEmail(newEmail: string): void {
    this.email = newEmail;
    console.log(`[System] Email updated for ${this.username}.`);
  }
}

// --- Main Execution ---

// Instantiate a new object
const user1 = new User(101, "jdoe", "jdoe@example.com");

// Interacting with the object via public methods
user1.displayProfile();
user1.updateEmail("john.doe@newdomain.com");
user1.displayProfile();
