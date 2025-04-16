//+------------------------------------------------------------------+
//|                                                script-sample.mq5 |
//|                                   Copyright 2016, Andrey Osorgin |
//+------------------------------------------------------------------+
//|                     The MIT License (MIT)                        |
//|                                                                  |
//| Permission is hereby granted, free of charge, to any person      |
//| obtaining a copy of this software and associated documentation   |
//| files (the "Software"), to deal in the Software without          |
//| restriction, including without limitation the rights to use,     |
//| copy, modify, merge, publish, distribute, sublicense, and/or sell|
//| copies of the Software, and to permit persons to whom the        |
//| Software is furnished to do so, subject to the following         |
//| conditions:                                                      |
//|                                                                  |
//| The above copyright notice and this permission notice shall be   |
//| included in all copies or substantial portions of the Software.  |
//|                                                                  |
//| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  |
//| EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  |
//| OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND         |
//| NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT      |
//| HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,     |
//| WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     |
//| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    |
//| OTHER DEALINGS IN THE SOFTWARE.                                  |
//|                                                                  |
//| A copy of the MIT License (MIT) is available at                  |
//| https://opensource.org/licenses/MIT                              |
//+------------------------------------------------------------------+
#property version   "1.00"
#property script_show_inputs

#include <Trade\Trade.mqh>

input int StopLoss=100; // Stop Loss
input int TakeProfit=100; // Take Profit
//+------------------------------------------------------------------+
//| Script program start function                                    |
//+------------------------------------------------------------------+
void OnStart()
  {
   CTrade trade;
//---
   long stoplevel=SymbolInfoInteger(Symbol(),SYMBOL_TRADE_STOPS_LEVEL);
   Print("Minimum stop level is: ",stoplevel);
   double ask=SymbolInfoDouble(Symbol(),SYMBOL_ASK);
   double bid=SymbolInfoDouble(Symbol(),SYMBOL_BID);
   double sl = NormalizeDouble(bid - StopLoss*Point(),Digits());
   double tp = NormalizeDouble(ask + TakeProfit*Point(),Digits());
//---
   bool result=trade.Buy(0.01,Symbol(),ask,sl,tp,"test");
//---
   Print("Success? ",result);
  }
//+------------------------------------------------------------------+
