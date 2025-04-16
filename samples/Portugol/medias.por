
/* CLIQUE NO SINAL DE "+", À ESQUERDA, PARA EXIBIR A DESCRIÇÃO DO EXEMPLO
 *  
 * Copyright (C) 2014 - UNIVALI - Universidade do Vale do Itajaí
 * 
 * Este arquivo de código fonte é livre para utilização, cópia e/ou modificação
 * desde que este cabeçalho, contendo os direitos autorais e a descrição do programa, 
 * seja mantido.
 * 
 * Se tiver dificuldade em compreender este exemplo, acesse as vídeoaulas do Portugol 
 * Studio para auxiliá-lo:
 * 
 * https://www.youtube.com/watch?v=K02TnB3IGnQ&list=PLb9yvNDCid3jQAEbNoPHtPR0SWwmRSM-t
 * 
 * Descrição:
 * 
 * 	Este exemplo pede ao usuário que informe três médias. Logo após, calcula 
 * 	e exibe a média final destas notas. Por últmo, verifica se alguma das 
 * 	médias parciais é menor que a média final e a exibe (caso exista).
 * 
 * Autores:
 * 
 * 	Giordana Maria da Costa Valle
 * 	Carlos Alexandre Krueger
 * 	
 * Data: 01/06/2013
 */

programa
{
	inclua biblioteca Matematica --> mat

	funcao inicio ()
	{
		real m1, m2, m3, media

		escreva ("Informe a média 1: " )
		leia (m1)
		escreva( "Informe a média 2: ")
		leia (m2)
		escreva ("Informe a média 3: ")
		leia (m3)

		media = (m1 + m2 + m3) / 3 

		limpa()
		escreva ("A média final é: ", mat.arredondar(media, 2), "\n\n")


		se (m1 < media)
		{ 
			escreva ("A média 1 é menor que a média final\n") 
		}

		se (m2 < media) 
		{
			escreva ("A média 2 é menor que a média final\n")
		}

		se (m3 < media)
		{
			escreva ("A média 3 é menor que a média final\n")
		}		
	}
}
