*/**
* The MIT License (MIT)
* Copyright (c) 2026 Fernando Ghinzelli
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

* > Natural Subroutine: FINANC01
* > Description: Processamento de faturas e cálculo de juros
************************************************************************
DEFINE DATA
GLOBAL USING G_FINANC         /* Referência a Global Data Area [cite: 6] */
PARAMETER                     /* Parâmetros de entrada/saída  */
  01 #ST-FATURA      (A1)
  01 #ID-CLIENTE     (N8)
  01 #VALOR-TOTAL    (P13.2)
LOCAL                         /* Variáveis locais do módulo  */
  01 #DATA-ATUAL     (D)
  01 #DIAS-ATRASO    (I4)
  01 #TAXA-JUROS     (P1.3) INIT <0.050>
  01 #JUROS-CALC     (P13.2)
  01 #I              (I2)
  01 faturas VIEW OF FATURAS-FILE /* View de banco de dados  */
    02 STATUS
    02 DATA-VENCIMENTO
    02 VALOR
  01 #MSG-ERRO       (A50)
END-DEFINE                    /* Fim da definição de dados  */

* ----------------------------------------------------------------------
* LÓGICA PRINCIPAL DA SUB-ROTINA
* ----------------------------------------------------------------------
DEFINE SUBROUTINE CALC-FINANCEIRO /* Início da sub-rotina [cite: 8] */

  ASSIGN #DATA-ATUAL = *DATX  /* Variável de sistema para data  */
  RESET #VALOR-TOTAL #JUROS-CALC /* Limpa variáveis  */

  /* Busca faturas pendentes para o cliente */
  FIND faturas WITH CLIENTE-ID = #ID-CLIENTE
    WHERE STATUS = #ST-FATURA

    IF faturas.DATA-VENCIMENTO < #DATA-ATUAL
      /* Cálculo de dias em atraso */
      COMPUTE #DIAS-ATRASO = #DATA-ATUAL - faturas.DATA-VENCIMENTO

      /* Regra de negócio: Juros simples por dia */
      #JUROS-CALC := (faturas.VALOR * #TAXA-JUROS) * #DIAS-ATRASO
      
      ADD #JUROS-CALC TO faturas.VALOR
      ADD faturas.VALOR TO #VALOR-TOTAL

      /* Atualiza o registro no Adabas */
      UPDATE (0046)           /* Update do registro atual  */
      
      DISPLAY 'FATURA PROCESSADA:' faturas.DATA-VENCIMENTO
              'JUROS:' #JUROS-CALC
    ELSE
      ADD faturas.VALOR TO #VALOR-TOTAL
    END-IF

  END-FIND                    /* Fim do loop de busca  */

  /* Tratamento caso não encontre registros */
  IF *COUNTER = 0             /* Variável de sistema: contador  */
    COMPRESS 'NENHUMA FATURA PARA CLIENTE' #ID-CLIENTE INTO #MSG-ERRO
    INPUT (AD=RO) #MSG-ERRO
  END-IF

END-SUBROUTINE                /* Encerra a sub-rotina [cite: 8] */

END                           /* Fim do objeto Natural  */