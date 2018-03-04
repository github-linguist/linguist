/2017.10.20 support taq3.0b
/ https://list.theice.com/t/92262/395348/57007/0/
/2016.10.18 support taq2.2a
/ http://www.nyxdata.com/doc/247075

if[(3.4>.z.K)|2016.09.26>.z.k;-1"kdb+ 3.4 more recent than 2016.09.26 required";exit 1];
k)o:.Q.opt .z.x;F:F@&(_F:!src:`$":",*.Q.x)like;S:`/:src,;dst:`:tq;D:"I"$-8#$*F;
if[1>count .Q.x;-1">q ",(string .z.f)," SRC";exit 1];

k)s16:{f:{x[&x=" "]:".";x};update`$Symbol from update Symbol:f'Symbol from x where Symbol like"* *"}
k)psym:{if[^@[@[;`Symbol;`p#];x;`];0N!x@&~(x?x)=!#x@:&~=':x@:`Symbol]}
k)adsftg2:{[a;dpt;xom;f;tw;g] d::*dpt;x::*xom
  p:*`/:(,.Q.par . dpt),`;wf:$[(nqt:~`quote=dpt 2)|"A"=c:(|$x)9;:;,];wp:nqt|"Z"=c
  t:.Q.en[d]@+g f!.:+s16 -1_tw 0:0N!x
  .[p;();wf;t];if[wp;psym p]}

k)foo2:{[t;tf;tt;tg;x]adsftg2[0;(dst;"D"$-8#$x;t);(S x;0;0);tf;tt;tg]}

/ 2.2
nh2:`Time`Exchange`Symbol`Bid_Price`Bid_Size`Offer_Price`Offer_Size`Quote_Condition`Sequence_Number`National_BBO_Ind`FINRA_BBO_Indicator`FINRA_ADF_MPID_Indicator`Quote_Cancel_Correction`Source_Of_Quote`NBBO_Quote_Condition`Best_Bid_Exchange`Best_Bid_Price`Best_Bid_Size`Best_Bid_FINRA_Market_Maker_ID`Best_Offer_Exchange`Best_Offer_Price`Best_Offer_Size`Best_Offer_FINRA_Market_Maker_ID`LULD_Indicator`LULD_NBBO_Indicator`SIP_Generated_Message_Identifier`Participant_Timestamp`FINRA_ADF_Timestamp;
nf2:("NC*EHEHCIHHHCCCCEH*CFH*CCCNN";enlist"|"); ng2:{x};
/ 3.0
nh2:`Time`Exchange`Symbol`Bid_Price`Bid_Size`Offer_Price`Offer_Size`Quote_Condition`Sequence_Number`National_BBO_Ind`FINRA_BBO_Indicator`FINRA_ADF_MPID_Indicator`Quote_Cancel_Correction`Source_Of_Quote`BestBidQuoteCondition`Best_Bid_Exchange`Best_Bid_Price`Best_Bid_Size`Best_Bid_FINRA_Market_Maker_ID`Best_Offer_Quote_Condition`Best_Offer_Exchange`Best_Offer_Price`Best_Offer_Size`Best_Offer_FINRA_Market_Maker_ID`LULD_Indicator`LULD_NBBO_Indicator`SIP_Generated_Message_Identifier`Participant_Timestamp`FINRA_ADF_Timestamp`Security_Status_Indicator
nf2:("NC*EHEHCIHHHCCCCEH*CCFH*CCCNNC";enlist"|"); ng2:{x};

/ 2.2 and 3.0
th2:`Time`Exchange`Symbol`SaleCondition`TradeVolume`TradePrice`TradeStopStockIndicator`TradeCorrectionIndicator`SequenceNumber`TradeId`SourceofTrade`TradeReportingFacility`ParticipantTimestamp`TradeReportingFacilityTRFTimestamp`TradeThroughExemptIndicator;
tf2:("NC*SHEBHI*CBNNB";enlist"|"); tg2:{x};

/ 2.2
qh2:`Time`Exchange`Symbol`Bid_Price`Bid_Size`Offer_Price`Offer_Size`Quote_Condition`Sequence_Number`National_BBO_Ind`FINRA_BBO_Indicator`FINRA_ADF_MPID_Indicator`Quote_Cancel_Correction`Source_Of_Quote`Retail_Interest_Indicator`Short_Sale_Restriction_Indicator`LULD_BBO_Indicator`SIP_Generated_Message_Identifier`National_BBO_LULD_Indicator`Participant_Timestamp`FINRA_ADF_Timestamp`FINRA_ADF_Market_Participant_Quote_Indicator;
qf2:("NC*EHFHCIHHCCCCCCCCNNC";enlist"|"); qg2:{x};
/ 3.0
qh2:`Time`Exchange`Symbol`Bid_Price`Bid_Size`Offer_Price`Offer_Size`Quote_Condition`Sequence_Number`National_BBO_Ind`FINRA_BBO_Indicator`FINRA_ADF_MPID_Indicator`Quote_Cancel_Correction`Source_Of_Quote`Retail_Interest_Indicator`Short_Sale_Restriction_Indicator`LULD_BBO_Indicator`SIP_Generated_Message_Identifier`National_BBO_LULD_Indicator`Participant_Timestamp`FINRA_ADF_Timestamp`FINRA_ADF_Market_Participant_Quote_Indicator`Security_Status_Indicator
qf2:("NC*EHFHCIHHCCCCCCCCNNCC";enlist"|"); qg2:{x};

if[(count Q:F"splits_us_all_bbo_*[0-9]")within 1 25;-1"missing quote splits";exit 1];
\ts {foo2[`quote;qh2;qf2;qg2]x}each .q.asc Q;
\ts {foo2[`trade;th2;tf2;tg2]x}each F"eqy_us_all_trade_[0-9]*";
\ts {foo2[`nbbo; nh2;nf2;ng2]x}each F"eqy_us_all_nbbo_[0-9]*";

\
http://www.nyxdata.com/Data-Products/Daily-TAQ