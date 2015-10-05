#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright © 2013 Martin Ueding <dev@martin-ueding.de>

import argparse
import matplotlib.pyplot as pl
import numpy as np
import scipy.optimize as op
from prettytable import PrettyTable

__docformat__ = "restructuredtext en"

# Sensitivität der Thermosäule
S = 30e-6

def phif(U):
    return U / S

def main():
    options = _parse_args()

    V = 1000

    data = np.genfromtxt("a-leer.csv", delimiter="\t")
    t = data[:,0]
    U = data[:,1] / V / 1000
    U_err = 0.7e-3 / V

    offset = np.mean(U[-3:])

    x = np.linspace(min(t), max(t))
    y = np.ones(x.size) * offset
    pl.plot(x, y * 10**6, label="Offset")

    print "Offset: {:.3g} V".format(offset)

    pl.errorbar(t, U * 10**6, yerr=U_err * 10**6, linestyle="none", marker="+",
                label="Messdaten")
    pl.grid(True)
    pl.legend(loc="best")
    pl.title(u"Bestimmung des Offsets")
    pl.xlabel(ur"Zeit $t / \mathrm{s}$")
    pl.ylabel(ur"Thermospannung $U / \mathrm{\mu V}$")
    pl.savefig("Plot_a-leer.pdf")
    pl.clf()

    V = 100

    data = np.genfromtxt("a-Lampe.csv", delimiter="\t")
    t = data[:,0]
    U = data[:,1] / V / 1000 - offset
    U_err = 0.7e-3 / V

    x = np.linspace(min(t), max(t))
    y = np.ones(x.size) * max(U) * 0.9
    pl.plot(x, y * 10**6, label=ur"$90\%$")

    pl.errorbar(t, U * 10**6, yerr=U_err * 10**6, linestyle="none", marker="+",
                label="Messdaten")
    pl.grid(True)
    pl.legend(loc="best")
    pl.title(u"Bestimmung der Ansprechzeit")
    pl.xlabel(ur"Zeit $t / \mathrm{s}$")
    pl.ylabel(ur"Thermospannung $U / \mathrm{\mu V}$")
    pl.savefig("Plot_a-Lampe.pdf")
    pl.clf()

    # Lesliewürfel
    print """
Lesliewürfel
============
"""

    glanz = np.genfromtxt("b-glanz.csv", delimiter="\t")
    matt = np.genfromtxt("b-matt.csv", delimiter="\t")
    schwarz = np.genfromtxt("b-schwarz.csv", delimiter="\t")
    weiss = np.genfromtxt("b-weiss.csv", delimiter="\t")

    T0 = 19.0 + 273.15
    T0_err = 1.0

    glanz[:,0] += 273.15
    matt[:,0] += 273.15
    schwarz[:,0] += 273.15
    weiss[:,0] += 273.15

    glanz[:,1] /= 1000 * V
    matt[:,1] /= 1000 * V
    schwarz[:,1] /= 1000 * V
    weiss[:,1] /= 1000 * V

    glanz[:,1] -= offset
    matt[:,1] -= offset
    schwarz[:,1] -= offset
    weiss[:,1] -= offset

    glanz_phi = phif(glanz[:,1])
    matt_phi = phif(matt[:,1])
    schwarz_phi = phif(schwarz[:,1])
    weiss_phi = phif(weiss[:,1])

    T_err = 0.3

    sigma = 5.670373e-8

    def boltzmann(T, epsilon, offset):
        return epsilon * sigma * T**4 + offset

    glanz_popt, glanz_pconv = op.curve_fit(boltzmann, glanz[:,0], glanz_phi)
    matt_popt, matt_pconv = op.curve_fit(boltzmann, matt[:,0], matt_phi)
    schwarz_popt, schwarz_pconv = op.curve_fit(boltzmann, schwarz[:,0], schwarz_phi)
    weiss_popt, weiss_pconv = op.curve_fit(boltzmann, weiss[:,0], weiss_phi)

    glanz_x = np.linspace(min(glanz[:,0]), max(glanz[:,0]))
    glanz_y = boltzmann(glanz_x, *glanz_popt)
    pl.plot(glanz_x, glanz_y, label="Fit glanz", color="gold")

    matt_x = np.linspace(min(matt[:,0]), max(matt[:,0]))
    matt_y = boltzmann(matt_x, *matt_popt)
    pl.plot(matt_x, matt_y, label="Fit matt", color="yellow")

    schwarz_x = np.linspace(min(schwarz[:,0]), max(schwarz[:,0]))
    schwarz_y = boltzmann(schwarz_x, *schwarz_popt)
    pl.plot(schwarz_x, schwarz_y, label="Fit schwarz", color="black")

    weiss_x = np.linspace(min(weiss[:,0]), max(weiss[:,0]))
    weiss_y = boltzmann(weiss_x, *weiss_popt)
    pl.plot(weiss_x, weiss_y, label="Fit weiss", color="gray")

    print "glanz ε = {:.3g} ± {:.3g}".format(glanz_popt[0], np.sqrt(glanz_pconv.diagonal()[0]))
    print "glanz offset = {:.3g} ± {:.3g}".format(glanz_popt[1], np.sqrt(glanz_pconv.diagonal()[1]))
    print "matt ε = {:.3g} ± {:.3g}".format(matt_popt[0], np.sqrt(matt_pconv.diagonal()[0]))
    print "matt offset = {:.3g} ± {:.3g}".format(matt_popt[1], np.sqrt(matt_pconv.diagonal()[1]))
    print "schwarz ε = {:.3g} ± {:.3g}".format(schwarz_popt[0], np.sqrt(schwarz_pconv.diagonal()[0]))
    print "schwarz offset = {:.3g} ± {:.3g}".format(schwarz_popt[1], np.sqrt(schwarz_pconv.diagonal()[1]))
    print "weiss ε = {:.3g} ± {:.3g}".format(weiss_popt[0], np.sqrt(weiss_pconv.diagonal()[0]))
    print "weiss offset = {:.3g} ± {:.3g}".format(weiss_popt[1], np.sqrt(weiss_pconv.diagonal()[1]))

    pl.errorbar(glanz[:,0], glanz_phi, xerr=T_err, yerr=U_err/S,
                label="glanz", color="gold", linestyle="none")
    pl.errorbar(matt[:,0], matt_phi, xerr=T_err, yerr=U_err/S,
                label="matt", color="yellow", linestyle="none")
    pl.errorbar(schwarz[:,0], schwarz_phi, xerr=T_err, yerr=U_err/S,
                label="schwarz", color="black", linestyle="none")
    pl.errorbar(weiss[:,0], weiss_phi, xerr=T_err, yerr=U_err/S,
                label="weiss", color="gray", linestyle="none")

    header = ["T / K", "Phi/F in W/m^2", "Fehler T", "Fehler Phi/F"]

    print """
Tabellen für den Lesliewürfel-Plot
----------------------------------
"""

    print "Glanz"
    glanz_table = PrettyTable(header)
    for row in zip(glanz[:,0], glanz_phi, np.ones(glanz[:,0].size)*T_err, np.ones(glanz_phi.size)*U_err/S):
        glanz_table.add_row(row)
    print glanz_table
    print

    print "Matt"
    matt_table = PrettyTable(header)
    for row in zip(matt[:,0], matt_phi, np.ones(matt[:,0].size)*T_err, np.ones(matt_phi.size)*U_err/S):
        matt_table.add_row(row)
    print matt_table
    print

    print "Schwarz"
    schwarz_table = PrettyTable(header)
    for row in zip(schwarz[:,0], schwarz_phi, np.ones(schwarz[:,0].size)*T_err, np.ones(schwarz_phi.size)*U_err/S):
        schwarz_table.add_row(row)
    print schwarz_table
    print

    print "Weiß"
    weiss_table = PrettyTable(header)
    for row in zip(weiss[:,0], weiss_phi, np.ones(weiss[:,0].size)*T_err, np.ones(weiss_phi.size)*U_err/S):
        weiss_table.add_row(row)
    print weiss_table
    print

    epsilon = 0.1

    x = np.linspace(min([min(x) for x in [glanz[:,0], matt[:,0], schwarz[:,0],
                                          weiss[:,0]]]),
                    max([max(x) for x in [glanz[:,0], matt[:,0], schwarz[:,0],
                                          weiss[:,0]]]),
                    100)
    offset = - epsilon * sigma * T0**4
    print "ideal offset = {:.3g}".format(offset)
    y = boltzmann(x, epsilon, offset)
    pl.plot(x, y, label=ur"$\epsilon = 0.1$")


    pl.grid(True)
    pl.title(u"Lesliewürfel")
    pl.xlabel(ur"Temperatur $T / \mathrm{K}$")
    pl.ylabel(ur"Strahlungsfluss $\frac{\Phi}{F} / \mathrm{\frac{W}{m^2}}$")
    pl.legend(loc="best", prop={"size": 12})
    pl.savefig("Plot_b.pdf")
    pl.clf()

    # Aufgabe c
    print """
Aufgabe c
=========
    """

    data = np.genfromtxt("c-erste.csv", delimiter="\t")
    d = data[:,0] / 100
    U = data[:,1] / V
    phi = phif(U)

    def c(x, a, b):
        return a*x + b


    dx = d**(-2)
    dy = phi

    dx_err = np.abs(-2 * d**(-3)) * 0.001
    dy_err = 0.001 / S

    popt, pconv = op.curve_fit(c, dx, dy)
    x = np.linspace(min(dx), max(dx))
    y = c(x, *popt)
    pl.plot(x, y, label="Fit")

    print "Fitparameter"
    print "a", popt[0], "±", np.sqrt(pconv.diagonal()[0])
    print "b", popt[1], "±", np.sqrt(pconv.diagonal()[1])

    pl.errorbar(dx, dy, xerr=dx_err, yerr=dy_err, linestyle="none",
                marker="+", label="Messdaten")
    pl.grid(True)
    pl.title(u"Halogenlampe bei verschiedenen Abständen")
    pl.xlabel(ur"Abstand $d^{-2} / \mathrm{m^{-2}}$")
    pl.ylabel(ur"Strahlungsfluss $\frac{\Phi}{F} / \mathrm{\frac{W}{m^2}}$")
    pl.legend(loc="best")
    pl.savefig("Plot_c-erste.pdf")
    pl.clf()

    print
    print "Tabelle für Aufgabe c"
    fields = ["d^-2 in m^-2", "Phi/F in W/m^2", "Fehler d^-2", "Fehler Phi/F"]
    table = PrettyTable(fields)
    table.align = "l"
    for row in zip(dx, dy, dx_err, np.ones(dy.size)*dy_err):
        table.add_row(row)
    print table
    print

    data = np.genfromtxt("c-zweite.csv", delimiter="\t")
    U1 = data[:,0]
    I1 = data[:,1]
    U2 = data[:,2] / V

    U_err = 0.001
    I_err = 0.01

    p = U1 * I1
    R = U1 / I1
    R_err = np.sqrt(
        (1/I1 * U_err)**2
        + (U1/I1**2 * I_err)**2
    )

    phi = phif(U2)
    phi_err = U_err / S

    alpha = 4.82e-3
    beta = 6.76e-7

    R0 = 0.35
    R0_err = 0.05

    T = (-alpha*R0 + np.sqrt(R0)*np.sqrt(4*beta*R + alpha**2*R0 - 4*beta*R0) +
 2*beta*R0*T0)/(2*beta*R0)

    popt, pconv = op.curve_fit(boltzmann, T, phi, sigma=phi_err)
    x = np.linspace(min(T), max(T))
    y = boltzmann(x, *popt)
    pl.plot(x, y, label="Fit")

    epsilon = popt[0]
    epsilon_err = np.sqrt(pconv.diagonal()[0])

    print "ε = {:.3g} ± {:.3g}".format(epsilon, epsilon_err)

    f1 = (1/(np.sqrt(R0)*np.sqrt(4*beta*R + alpha**2*R0 - 4*beta*R0))) * R_err
    f2 = T0_err
    f3 = ((-alpha + ((alpha**2 - 4*beta)*np.sqrt(R0))/( 2*np.sqrt(4*beta*R + alpha**2*R0 - 4*beta*R0)) + np.sqrt( 4*beta*R + alpha**2*R0 - 4*beta*R0)/(2*np.sqrt(R0)) + 2*beta*T0)/( 2*beta*R0) - (-alpha*R0 + np.sqrt(R0)*np.sqrt(4*beta*R + alpha**2*R0 - 4*beta*R0) + 2*beta*R0*T0)/( 2*beta*R0**2)) * R0_err

    T_err = np.sqrt(f1**2 + f2**2 + f3**2)

    pl.errorbar(T, phi, xerr=T_err, yerr=phi_err, label="Messdaten",
                linestyle="none", marker="+")
    pl.grid(True)
    pl.legend(loc="best")
    pl.title(u"Halogenlampe bei verschiedenen Leistungen")
    pl.xlabel(u"Temperatur $T / \mathrm{K}$")
    pl.ylabel(ur"Strahlungsfluss $\frac{\Phi}{F} / \mathrm{\frac{W}{m^2}}$")
    pl.savefig("Plot_c-zweite.pdf")
    pl.clf()

def _parse_args():
    """
    Parses the command line arguments.

    :return: Namespace with arguments.
    :rtype: Namespace
    """
    parser = argparse.ArgumentParser(description="")
    #parser.add_argument("args", metavar="N", type=str, nargs="*", help="Positional arguments.")
    #parser.add_argument("", dest="", type="", default=, help=)
    #parser.add_argument("--version", action="version", version="<the version>")

    return parser.parse_args()


if __name__ == "__main__":
    main()
