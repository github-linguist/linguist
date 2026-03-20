afficher("Bonjour le monde!")

déf fibonacci(n):
    si n <= 1:
        retourner n
    retourner fibonacci(n-1) + fibonacci(n-2)

pour i dans intervalle(10):
    afficher(fibonacci(i))