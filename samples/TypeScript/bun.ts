#!/usr/bin/env bun

/**
 * @fileoverview Demonstration of OOP principles in TypeScript using Bun.
 * Run this script with: ./bank.ts
 */

// --- Abstraction: Interface Defining Behaviors ---
interface IAccount {
    deposit(amount: number): void;
    withdraw(amount: number): void;
    getBalance(): number;
}

// --- Encapsulation: Base Class ---
class BankAccount implements IAccount {
    // Private properties: Restricted access only within this class [2, 11]
    private balance: number;
    private readonly accountHolder: string;

    constructor(accountHolder: string, initialBalance: number = 0) {
        this.accountHolder = accountHolder;
        this.balance = initialBalance;
    }

    // Public Method
    public deposit(amount: number): void {
        if (amount > 0) {
            this.balance += amount;
            console.log(`[${this.accountHolder}] Deposited: $${amount}`);
        }
    }

    // Public Method
    public withdraw(amount: number): void {
        if (amount <= this.balance) {
            this.balance -= amount;
            console.log(`[${this.accountHolder}] Withdrew: $${amount}`);
        } else {
            console.log(`[${this.accountHolder}] Insufficient funds!`);
        }
    }

    // Public Method
    public getBalance(): number {
        return this.balance;
    }

    // Protected access: Available to subclasses [2]
    protected getAccountHolder(): string {
        return this.accountHolder;
    }
}

// --- Inheritance: Subclass Derived from BankAccount ---
class SavingsAccount extends BankAccount {
    private interestRate: number;

    constructor(accountHolder: string, initialBalance: number, interestRate: number) {
        // Calling super constructor [10]
        super(accountHolder, initialBalance);
        this.interestRate = interestRate;
    }

    // --- Polymorphism: Overriding Method ---
    public withdraw(amount: number): void {
        console.log(`[${this.getAccountHolder()}] Savings withdrawal requested.`);
        super.withdraw(amount); // Reusing base functionality [10]
    }

    public applyInterest(): void {
        const interest = this.getBalance() * this.interestRate;
        this.deposit(interest);
        console.log(`[${this.getAccountHolder()}] Interest applied: $${interest}`);
    }
}

// --- Execution Loop ---
const mySavings = new SavingsAccount("Alice", 1000, 0.05);

mySavings.deposit(500);
mySavings.applyInterest();
mySavings.withdraw(200);

console.log(`Final Balance: $${mySavings.getBalance()}`);
