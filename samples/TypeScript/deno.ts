#!/usr/bin/env -S deno run --allow-env
/**
 * @file user.ts
 * @description Demonstrates Object-Oriented Programming (OOP) in Deno using TypeScript.
 * Features encapsulated data, constructors, and methods.
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
