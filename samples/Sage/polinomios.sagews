# -*- coding: utf-8 -*-
#
#   Funciones en Python/Sage para el trabajo con polinomios con una
#   incógnita (x).
#
#   Copyright (C) 2014-2015, David Abián <davidabian [at] davidabian.com>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but WITHOUT
#   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
#   more details.
#
#   You should have received a copy of the GNU General Public License along with
#   this program.  If not, see <http://www.gnu.org/licenses/>.

def pols (grado=-1, K=GF(2), mostrar=False):
    """Devuelve la lista de polinomios constantes y no constantes de
    coeficientes mónicos y grado igual o menor que el especificado.
    Si el grado indicado no es válido, devuelve una lista vacía.
    """
    lpols = []
    if not grado.is_integer():
        grado = grado.round()
    if grado >= 0:
        var('x')
        xs = vector([(x^i) for i in range(grado+1)])
        V = VectorSpace(K,grado+1)
        lpols = [cs*xs for cs in V]
        if mostrar:
            for pol in lpols:
                print pol
    return lpols

def polsNoCtes (grado=-1, K=GF(2), mostrar=False):
    """Devuelve la lista de polinomios no constantes de coeficientes mónicos y
    grado igual o menor que el especificado.
    Si el grado indicado no es válido, devuelve una lista vacía.
    """
    lpols = []
    if not grado.is_integer():
        grado = grado.round()
    if grado >= 0:
        var('x')
        xs = vector([(x^i) for i in range(grado+1)])
        for cs in K^(grado+1):
            if cs[:grado] != vector(grado*[0]): # no constantes
                lpols += [cs*xs]
        if mostrar:
            for pol in lpols:
                print pol
    return lpols

def polsMismoGrado (grado=-1, K=GF(2), mostrar=False):
    """Devuelve la lista de polinomios de coeficientes mónicos del grado
    especificado.
    Si el grado indicado no es válido, devuelve una lista vacía.
    """
    lpols = []
    if not grado.is_integer():
        grado = grado.round()
    if grado >= 0:
        var('x')
        xs = vector([(x^(grado-i)) for i in [0..grado]])
        for cs in K^(grado+1):
            if cs[0] != 0: # polinomios del mismo grado
                lpols += [cs*xs]
        if mostrar:
            for pol in lpols:
                print pol
    return lpols

def excluirReducibles (lpols=[], mostrar=False):
    """Filtra una lista dada de polinomios de coeficientes mónicos y devuelve
    aquellos irreducibles.
    """
    var('x')
    irreds = []
    for p in lpols:
        fp = (p.factor_list())
        if len(fp) == 1 and fp[0][1] == 1:
            irreds += [p]
    if mostrar:
        for pol in irreds:
            print pol
    return irreds

def vecPol (vec=random_vector(GF(2),0)):
    """Transforma los coeficientes dados en forma de vector en el polinomio
    que representan.
    
    Por ejemplo, con vecPol(vector([1,0,3,1])) se obtiene x³ + 3*x + 1.
    
    Para la función opuesta, véase polVec().
    """
    var('x')
    xs = vector([x^(len(vec)-1-i) for i in range(len(vec))])
    return vec*xs

def polVec (p=None):
    """Devuelve el vector de coeficientes del polinomio dado que acompañan a la
    incógnita x, de mayor a menor grado.
    
    Por ejemplo, con polVec(x^3 + 3*x + 1) se obtiene el vector (1, 0, 3, 1).
    
    Para la función opuesta, véase vecPol().
    """
    cs = []
    if p != None:
        var('x')
        p(x) = p
        for i in [0..p(x).degree(x)]:
            cs.append(p(x).coefficient(x,i))
        cs = list(reversed(cs))
    return vector(cs)

def completar2 (p=0):
    """Aplica el método de completar cuadrados en parábolas al polinomio dado de
    grado 2 y lo devuelve en su nueva forma.
    
    Si el polinomio dado no es válido, devuelve 0.
    
    Por ejemplo, con complCuad(3*x^2 + 12*x + 5) se obtiene 3*(x + 2)^2 - 7.
    """
    var('x')
    p(x) = p.expand()
    if p(x).degree(x) != 2:
        p(x) = 0
    else:
        cs = polVec(p(x))
        p(x) = cs[0]*(x+(cs[1]/(2*cs[0])))^2+(4*cs[0]*cs[2]-cs[1]^2)/(4*cs[0])
    return p(x)
