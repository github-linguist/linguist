
/**
 * This is a sample file for Linguist.
 * It's written in AdvPL, a xBase Language.
 *
 * Author: Arthur Helfstein Fragoso
 *
 * This script has the specific use of integrating between a financial institution
 * and other two companies in the process of creating Installment Bills for
 * customers.
 *
 * The functions are called from the ERP Protheus TOTVS.
 *
 **/



#Include "TOPCONN.ch"
#include "tbiconn.ch"
#Include "Protheus.ch"
#Include "rwmake.ch"
#Include "FileIO.ch"
#Include "json.ch"
#Include "utils.ch"


////////////////////////
// Faturando (Reparcelando)
// FA280
// FA280_01
//

User Function FA280()

	//Executado uma vez para cada parcela

	If cEmpAnt == '06'

		SE5->(dbSelectArea("SE5"))

		cSet3Filter := "SE5->E5_FATURA == SE1->E1_NUM"

		SE5->(dbSetFilter( {|| &cSet3Filter }, cSet3Filter ))
		SE5->(dbGoTOP())

		aOrig06Tit := {} // = Todos os Titulos que ser�o reparcelados
		nTotal := 0

		While SE5->(!EOF())
			AADD(aOrig06Tit, {SE5->E5_PREFIXO, SE5->E5_NUMERO, SE5->E5_VALOR})
			nTotal += SE5->E5_VALOR
			SE5->(dbSkip())
		End

		aNovoTitulo:= {;//{"E1_FILIAL"   ,SE1ORIG->E1_FILIAL ,Nil},;
		              ;//{"E1_PREFIXO"  ,SE1->E1_PREFIXO    ,Nil},;
		              {"E1_NUM"      ,SE1->E1_NUM        ,Nil},;
		              {"E1_TIPO"     ,SE1->E1_TIPO       ,Nil},;
		              {"E1_PARCELA"  ,SE1->E1_PARCELA    ,Nil},;
		              {"E1_NATUREZ"  ,SE1->E1_NATUREZ    ,Nil},;
		              {"E1_CLIENTE"  ,SE1->E1_CLIENTE    ,Nil},;
		              {"E1_LOJA"     ,SE1->E1_LOJA       ,Nil},;
		              {"E1_NRDOC"    ,SE1->E1_NRDOC      ,Nil},;
		              ;//{"E1_X_COD"    ,SE1->E1_NATUREZ    ,Nil},;
		              {"E1_EMISSAO"  ,SE1->E1_EMISSAO    ,Nil},;
		              {"E1_VENCTO"   ,SE1->E1_VENCTO     ,Nil},;
		              {"E1_VENCREA"  ,SE1->E1_VENCREA    ,Nil},;
		              ;//{"E1_VALOR"    ,SE1->E1_VALOR      ,Nil},;
		              ;//{"E1_SALDO"    ,SE1->E1_SALDO      ,Nil},;
		              ;//{"E1_VLCRUZ"   ,SE1->E1_VLCRUZ     ,Nil},;
		              {"E1_PORTADO"  ,SE1->E1_PORTADO    ,Nil},;
		              {"E1_FATURA"   ,SE1->E1_FATURA     ,Nil},;
		              {"E1_X_DTPAV"  ,SE1->E1_X_DTPAV    ,Nil},;
		              {"E1_X_DTSAV"  ,SE1->E1_X_DTSAV    ,Nil},;
		              {"E1_X_DTTAV"  ,SE1->E1_X_DTTAV    ,Nil},;
		              {"E1_X_DTSPC"  ,SE1->E1_X_DTSPC    ,Nil},;
		              {"E1_X_DTPRO"  ,SE1->E1_X_DTPRO    ,Nil},;
		              {"E1_NUMBCO"   ,SE1->E1_NUMBCO     ,Nil},;
		              {"E1_X_DUDME"  ,SE1->E1_X_DUDME    ,Nil},;
		              {"E1_X_TIPOP"  ,SE1->E1_X_TIPOP    ,Nil},;
		              {"E1_X_DTCAN"  ,SE1->E1_X_DTCAN    ,Nil},;
		              {"E1_X_MOTIV"  ,SE1->E1_X_MOTIV    ,Nil},;
		              {"E1_X_DESPC"  ,SE1->E1_X_DESPC    ,Nil},;
		              {"E1_NUMNOTA"  ,SE1->E1_NUMNOTA    ,Nil},;
		              {"E1_SERIE"    ,SE1->E1_SERIE      ,Nil},;
		              {"E1_X_DEPRO"  ,SE1->E1_X_DEPRO    ,Nil},;
		              {"E1_X_TPPAI"  ,SE1->E1_X_TPPAI    ,Nil},;
		              {"E1_X_CGC"    ,SE1->E1_X_CGC      ,Nil},;
		              {"E1_XTPEMP"   ,SE1->E1_XTPEMP     ,Nil},;
		              {"E1_X_CTRIM"  ,SE1->E1_X_CTRIM    ,Nil}}

		StartJob("U_FA280_01",getenvserver(),.T., SE1->E1_PREFIXO ,SE1->E1_NUM, SE1->E1_TIPO, SE1->E1_VALOR, aOrig06Tit, nTotal, SE1->E1_PARCELA, aNovoTitulo)

		SE5->(dbClearFilter())

	EndIf

Return nil


User Function FA280_01(cE1PREFIXO, cE1NUM, cE1TIPO, nE1Valor, aOrig06Tit, nTotal, cE1PARCELA, aNovoTitulo)
	Local nValPar := nil
	Local aTit05 := {}

	RpcSetType(3) // Nao consome licensa

	//Prepare Environment Empresa "01" Filial '0102'
	// Muda de empresa
	While !RpcSetEnv('01', '0102',,,,GetEnvServer(),{})
		Sleep(400)
	End

		nFileLog := u_OpenLog("\Logs\FA280_"+dToS(dDataBase)+".log")

		fWrite(nFileLog,"----- FA280 -----"+CRLF)

		fWrite(nFileLog,cE1NUM+CRLF)

		nParcelas := round(nTotal/nE1Valor, 0)

		cUltima := '0'+ chr(64+nParcelas)

		fWrite(nFileLog,"valor das parcelas: "+ cvaltochar(nE1Valor) +CRLF)
		fWrite(nFileLog,"parcelas: "+ cvaltochar(nParcelas) +CRLF)
		fWrite(nFileLog,"parcela atual: "+ cE1PARCELA +CRLF)
		fWrite(nFileLog,"ultima parcela: "+ cUltima +CRLF)

		n0102total := 0
		n0105total := 0

		//Loop entre todos os Titulos que serão Reparcelados

		For nI := 1 To len(aOrig06Tit)

			fWrite(nFileLog,"E5_NUMERO: "+aOrig06Tit[nI][2] +CRLF)

			cQuery := "select * from SE1010 where E1_PREFIXO = '"+ aOrig06Tit[nI][1] +"' and E1_NUM = '"+ aOrig06Tit[nI][2] +"' and  E1_TIPO = 'FAT' and D_E_L_E_T_ <> '*'"

			fWrite(nFileLog,cQuery +CRLF)

			If select("SE1ORIG") > 0
				SE1ORIG->(DbCloseArea())
			endif
			TcQuery cQuery New Alias 'SE1ORIG'
			dbSelectArea("SE1ORIG")
			SE1ORIG->(DBGOTOP())

			While SE1ORIG->(!EOF()) //Loop entre as duas filiais: 0102, 0105
				fWrite(nFileLog,"SE1ORIG loop: "+SE1ORIG->E1_FILIAL +CRLF)
				cFilAnt := SE1ORIG->E1_FILIAL

				//Faz a baixa
				if alltrim(SE1ORIG->E1_STATUS) == 'A'
					fWrite(nFileLog, SE1ORIG->E1_FILIAL+" : Fazendo baixa" +CRLF)

					aBaixa	:= {{"E1_FILIAL"   ,SE1ORIG->E1_FILIAL   ,Nil},;
					            {"E1_PREFIXO"  ,SE1ORIG->E1_PREFIXO  ,Nil},;
					            {"E1_NUM"      ,SE1ORIG->E1_NUM      ,Nil},;
					            {"E1_TIPO"     ,SE1ORIG->E1_TIPO     ,Nil},;
					            {"E1_PARCELA"  ,SE1ORIG->E1_PARCELA  ,Nil},;
					            {"E1_DESCONT"  ,SE1ORIG->E1_DESCONT  ,Nil},;
					            {"E1_JUROS"    ,SE1ORIG->E1_JUROS    ,Nil},;
					            {"E1_MULTA"    ,SE1ORIG->E1_MULTA    ,Nil},;
					            {"E1_VLRREAL"  ,SE1ORIG->E1_VLRREAL  ,Nil},;
					            {"AUTMOTBX"    ,"FAT"                ,Nil},;
					            {"AUTDTBAIXA"  ,date()               ,Nil},;
					            {"AUTDTCREDITO",date()               ,Nil},;
					            {"AUTHIST"     ,"Bx.Emis.Fat."+cE1NUM,Nil},;
					            {"AUTVALREC"   ,SE1ORIG->E1_VALOR    ,Nil}}


					lMsErroAuto:=.F. //reseta lMsErroAuto
					MSExecAuto ({|x,y| FINA070(x,y)},aBaixa, 3)

					If lMsErroAuto

						fWrite(nFileLog,SE1ORIG->E1_FILIAL+" : Não foi efetuada a baixa do titulo : "+CRLF+ MSErroString()+ CRLF + tojson(aBaixa) + CRLF)
						return
					else

						RECLOCK('SE5',.F.)
							E5_FATURA := cE1NUM
							E5_FATPREF:= cE1PREFIXO
							//E5_LA = S
							//E5_MOEDA = ''
							//E5_TXMOEDA = 1
						MSUNLOCK()
						RECLOCK('SE1',.F.)
							E1_FATURA := cE1NUM
							E1_FATPREF:= cE1PREFIXO
							E1_TIPOFAT:= cE1TIPO
							E1_FLAGFAT:= 'S'
							E1_DTFATUR:= dDataBase
						MSUNLOCK()

						fWrite(nFileLog,SE1ORIG->E1_FILIAL+" : baixa feita" +CRLF)
					endif

				endif

				//calcula valor total de cada filial para poder calcular a Fatura

				if SE1ORIG->E1_FILIAL == '0102'
					n0102total += SE1ORIG->E1_VALOR
				elseif SE1ORIG->E1_FILIAL == '0105'
					n0105total += SE1ORIG->E1_VALOR
				else
					fWrite(nFileLog,"Programa nao preparado para a filial "+SE1ORIG->E1_FILIAL +CRLF)
				endif

				SE1ORIG->(dbskip())

			End

		Next nI

		cFilAnt := '0102'

		fWrite(nFileLog,"Total 0102: "+cvaltochar(n0102total) +CRLF)
		fWrite(nFileLog,"Total 0105: "+cvaltochar(n0105total) +CRLF)

		n0102val := round(nE1Valor * n0102total/nTotal, 2)
		n0105val := nE1Valor - n0102val

		aFili := {}

		if n0102total > 0
			AADD(aFili,'0102')
		endif

		if n0105total > 0
			AADD(aFili,'0105')
		endif

		For nI := 1 To len(aFili)

			cQuery := "select COUNT(*) as QUANT, SUM(E1_VALOR) as TOTALINC from SE1010  where E1_NUM = '"+ cE1NUM +"'  and E1_FILIAL='"+ aFili[nI] +"' and E1_PREFIXO = '"+ cE1PREFIXO +"' and D_E_L_E_T_ <> '*'"

			If select("PARC") > 0
				PARC->(DbCloseArea())
			endif
			TcQuery cQuery New Alias 'PARC'
			dbSelectArea("PARC")

			//verificamos se estamos na ultima parcela
			if PARC->QUANT == nParcelas -1 //QUANT = quantidade de parcelas incluida
				fWrite(nFileLog,"Ultima Parcela"+CRLF)
				//o valor desta será o valor que resta
				nValPar := SE1ORIG->E1_VALOR - PARC->TOTALINC

				if aFili[nI] == '0102'
					n0102val := n0102total - PARC->TOTALINC
				elseif aFili[nI] == '0105'
					n0105val := n0105total - PARC->TOTALINC
				endif
			endif

		Next nI

		fWrite(nFileLog,"Total 0102: "+cvaltochar(n0102total) + "  -> Parcela de: "+cvaltochar(n0102val) +CRLF)
		fWrite(nFileLog,"Total 0105: "+cvaltochar(n0105total) + "  -> Parcela de: "+cvaltochar(n0105val) +CRLF)

		/////////////////

		For nI := 1 To len(aFili)

			if aFili[nI] == '0102'
				nValPar := n0102val
			elseif aFili[nI] == '0105'
				nValPar := n0105val
			endif

			aTitulo := ACLONE(aNovoTitulo)

			AADD(aTitulo, {"E1_PREFIXO" ,cE1PREFIXO          ,Nil})
			AADD(aTitulo, {"E1_FILIAL"  ,aFili[nI]           ,Nil})
			AADD(aTitulo, {"E1_VALOR"   ,nValPar             ,Nil})
			AADD(aTitulo, {"E1_SALDO"   ,nValPar             ,Nil})
			AADD(aTitulo, {"E1_VLCRUZ"  ,nValPar             ,Nil})

			lMsErroAuto := .F.

			if aFili[nI] == '0102'

				MSExecAuto({|x,y| FINA040(x,y)},aTitulo,3) //Inclusao

				If lMsErroAuto
					fWrite(nFileLog,"Erro " + CRLF)
					fWrite(nFileLog,"Erro ao incluir titulo: "+CRLF+ MSErroString()+ CRLF + tojson(aTitulo) + CRLF)
					return
				else
					fWrite(nFileLog,"Sucesso "+ CRLF)
					fWrite(nFileLog,"Titulo incluido: "+ aFili[nI] +" : " + cValToChar(nValPar) +CRLF)
				endif

			elseif aFili[nI] == '0105'
				fWrite(nFileLog,"Salvando titulos 05 para o final "+aFili[nI]+CRLF)
				//StartJob("U_JOBF040",getenvserver(),.T., SE1ORIG->E1_FILIAL, aTitulo)
				AADD(aTit05, aTitulo)
				//fWrite(nFileLog,"passou pela thread "+CRLF)
			else
				fWrite(nFileLog,"Erro, filial nao tratada "+aFili[nI]+CRLF)
			endif

		Next nI


	Reset Environment

	While !RpcSetEnv('01', '0105',,,,GetEnvServer(),{})
		Sleep(400)
	End

		For nI := 1 To len(aTit05)

			lMsErroAuto := .F.

			MSExecAuto({|x,y| FINA040(x,y)},aTit05[nI],3) //Inclusao

			If lMsErroAuto
				fWrite(nFileLog,"Erro " + CRLF)
				fWrite(nFileLog,"Erro ao incluir titulo: "+CRLF+ MSErroString()+ CRLF + tojson(aTit05[nI]) + CRLF)
				return
			else
				fWrite(nFileLog,"Sucesso "+ CRLF)
				fWrite(nFileLog,"Titulo incluido: "+CRLF)
			endif

		Next nI

	Reset Environment

	fClose(nFileLog)

Return




////////////////////////
// Cancelamento da Fatura (Cancelamento do Reparcelamento)
// F280PCAN
// JOBF280C
//

User Function F280PCAN()

	/**
	 * cFatCan  - numero da fatura
	 * cPrefCan - prefixo
	 * cTipoCan - tipo
	 **/

	If cEmpAnt == '06'

		StartJob("U_JOBF280C",getenvserver(),.T., cPrefCan, cFatCan, cTipoCan)

	EndIf

Return .T.


User Function JOBF280C(cPrefCan, cFatCan, cTipoCan)

	RpcSetType(3) // Nao consome licensa

	While !RpcSetEnv('01', '0102',,,,GetEnvServer(),{})
		Sleep(400)
	End

		nFileLog := u_OpenLog("\Logs\F280PCAN_"+dToS(dDataBase)+".log")

		fWrite(nFileLog,"----- F280PCAN -----"+CRLF)

		fWrite(nFileLog,"E1_PREFIXO = '"+ cPrefCan +"' and E1_NUM = '"+ cFatCan +"' and  E1_TIPO = '"+ cTipoCan +"'"+CRLF)

		cQuery := "select * from SE1010 where E1_PREFIXO = '"+ cPrefCan +"' and E1_NUM = '"+ cFatCan +"' and  E1_TIPO = '"+ cTipoCan +"' and D_E_L_E_T_ <> '*'"

		If select("SE1ORIG") > 0
			SE1ORIG->(DbCloseArea())
		endif
		TcQuery cQuery New Alias 'SE1ORIG'
		dbSelectArea("SE1ORIG")
		SE1ORIG->(DBGOTOP())

		While SE1ORIG->(!EOF()) //Loop entre todas as parcelas e filiais

			SE1->(dbselectarea("SE1"))
			SE1->(dbSetOrder(1))


			fWrite(nFileLog,"dbseek" + CRLF)
			if ! SE1->(dbSeek(SE1ORIG->E1_FILIAL+ SE1ORIG->E1_PREFIXO+ SE1ORIG->E1_NUM+ SE1ORIG->E1_PARCELA+ SE1ORIG->E1_TIPO))
				fWrite(nFileLog,"Erro dbseek" + CRLF)
				Alert("Erro. Verificar F280PCAN() - dbseek")
				fWrite(nFileLog,"Erro dbseek("+SE1ORIG->E1_FILIAL+ SE1ORIG->E1_PREFIXO+ SE1ORIG->E1_NUM+ SE1ORIG->E1_PARCELA+ SE1ORIG->E1_TIPO+")" + CRLF)
				return .F.
			endif

			cFilAnt := SE1ORIG->E1_FILIAL

			aFatura:= {{"E1_FILIAL"  ,SE1ORIG->E1_FILIAL         ,Nil},;
			           {"E1_PREFIXO" ,SE1ORIG->E1_PREFIXO        ,Nil},;
			           {"E1_NUM"     ,SE1ORIG->E1_NUM            ,Nil},;
			           {"E1_PARCELA" ,SE1ORIG->E1_PARCELA        ,Nil},;
			           {"E1_TIPO"    ,SE1ORIG->E1_TIPO           ,Nil}}

			lMsErroAuto := .F.

			MSExecAuto({|x,y| FINA040(x,y)},aFatura,5) //Exclus�o

			If lMsErroAuto
				fWrite(nFileLog,"Erro " + CRLF)
				fWrite(nFileLog,"Erro ao remover o titulo: "+CRLF+ MSErroString()+ CRLF + tojson(aFatura) + CRLF)
				Alert("Erro ao remover o titulo. Verificar F280PCAN()")
				return .F.
			else
				fWrite(nFileLog,"Sucesso "+ CRLF)
				fWrite(nFileLog,"Titulo removido" +CRLF)
			endif

			SE1ORIG->(dbskip())

		end


		/////////////////////////////////////////////
		///////  Cancela as baixas
		///

		fWrite(nFileLog,"- cancela baixas" + CRLF)

		cQuery := "select * from SE1060 where  E1_FATURA = '"+ cFatCan +"' and D_E_L_E_T_ <> '*'"

		If select("SE1ORIG") > 0
			SE1ORIG->(DbCloseArea())
		endif
		TcQuery cQuery New Alias 'SE1ORIG'
		dbSelectArea("SE1ORIG")
		SE1ORIG->(DBGOTOP())

		aFili := {"0102", "0105"}

		While SE1ORIG->(!EOF()) //Loop entre todas as parcelas e filiais

			SE1->(dbselectarea("SE1"))
			SE1->(dbSetOrder(1))

			For nI := 1 To len(aFili)

				cFilAnt := aFili[nI]

				fWrite(nFileLog,"dbseek" + CRLF)
				if ! SE1->(dbSeek(aFili[nI]+ SE1ORIG->E1_PREFIXO+ SE1ORIG->E1_NUM+ SE1ORIG->E1_PARCELA+ SE1ORIG->E1_TIPO))
					fWrite(nFileLog,"dbseek nao encontrou titulo para filial "+aFili[nI] + CRLF)
					fWrite(nFileLog,"dbseek('"+aFili[nI]+ SE1ORIG->E1_PREFIXO+ SE1ORIG->E1_NUM+ SE1ORIG->E1_PARCELA+ SE1ORIG->E1_TIPO+"')" + CRLF)
					LOOP
				endif

				nSE5Recno := u_RetSQLOne("select R_E_C_N_O_ from SE5010 where E5_FILIAL = '"+SE1->E1_FILIAL+"' and E5_PREFIXO = '"+SE1->E1_PREFIXO+"' and E5_TIPO = '"+SE1->E1_TIPO+"' and E5_NUMERO = '"+SE1->E1_NUM+"' "+;
				                         " and E5_FATURA = '"+SE1->E1_FATURA+"' and E5_FATPREF='"+SE1->E1_FATPREF+"' and D_E_L_E_T_ <> '*' ", "R_E_C_N_O_")

				//Removemos os Flags de Fatura para conseguirmos cancelar a baixa pelo FINA070
				RECLOCK('SE1',.F.)
					E1_FATURA := ''
					E1_FATPREF:= ''
					E1_TIPOFAT:= ''
					E1_FLAGFAT:= ''
					E1_DTFATUR:= StoD('')
			    MSUNLOCK()

			    SE5->(DbGoTo(nSE5Recno))
				RECLOCK('SE5',.F.)
					E5_MOTBX  := 'NOR'
					//E5_FATURA := ''
					//E5_FATPREF:= ''
				MSUNLOCK()

				aBaixa := {{"E1_FILIAL"   ,SE1->E1_FILIAL  ,Nil},;
				           {"E1_PREFIXO"  ,SE1->E1_PREFIXO ,Nil},;
				           {"E1_NUM"      ,SE1->E1_NUM     ,Nil},;
				           {"E1_TIPO"     ,SE1->E1_TIPO    ,Nil},;
				           {"E1_PARCELA"  ,SE1->E1_PARCELA ,Nil},;
				           {"E1_DESCONT"  ,SE1->E1_DESCONT ,Nil},;
				           {"E1_JUROS"    ,SE1->E1_JUROS   ,Nil},;
				           {"E1_MULTA"    ,SE1->E1_MULTA   ,Nil},;
				           {"E1_VLRREAL"  ,SE1->E1_VLRREAL ,Nil},;
				           {"AUTMOTBX"    ,"NOR"           ,Nil},;
				           {"AUTDTBAIXA"  ,date()          ,Nil},;
				           {"AUTDTCREDITO",date()          ,Nil},;
				           {"AUTHIST"     ,""              ,Nil},;
				           {"AUTVALREC"   ,SE1->E1_VALOR   ,Nil}}


				lMsErroAuto:=.F. //reseta lMsErroAuto
				MSExecAuto ({|x,y| FINA070(x,y)},aBaixa, 5)

				If lMsErroAuto
					fWrite(nFileLog,SE1->E1_FILIAL+" : Não foi efetuada o cancelamento de baixa : "+CRLF+ MSErroString()+ CRLF + tojson(aBaixa) + CRLF)
					return
				else
					fWrite(nFileLog,SE1->E1_FILIAL+" : cancelamento de baixa feito" +CRLF)
				endif

			Next nI

			SE1ORIG->(dbskip())
		end

	Reset Environment

Return
