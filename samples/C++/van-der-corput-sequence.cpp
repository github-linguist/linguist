#include <cmath>
#include <iostream>

double vdc(int n, double base = 2)
{
    double vdc = 0, denom = 1;
    while (n)
    {
        vdc += fmod(n, base) / (denom *= base);
        n /= base; // note: conversion from 'double' to 'int'
    }
    return vdc;
}

int main()
{
    for (double base = 2; base < 6; ++base)
    {
        std::cout << "Base " << base << "\n";
        for (int n = 0; n < 10; ++n)
        {
            std::cout << vdc(n, base) << " ";
        }
        std::cout << "\n\n";
    }
}
