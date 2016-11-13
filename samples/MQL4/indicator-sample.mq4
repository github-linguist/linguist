//+------------------------------------------------------------------+
//|                                             indicator-sample.mq4 |
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
#property strict

#property indicator_chart_window
#property indicator_plots 0
//+------------------------------------------------------------------+
//| Custom indicator initialization function                         |
//+------------------------------------------------------------------+
void OnInit(void)
  {
 //---
   }
//+------------------------------------------------------------------+
//| Bears Power                                                      |
//+------------------------------------------------------------------+
int OnCalculate(const int rates_total,
                const int prev_calculated,
                const datetime &time[],
                const double &open[],
                const double &high[],
                const double &low[],
                const double &close[],
                const long &tick_volume[],
                const long &volume[],
                const int &spread[])
  {
  Print("The number of bars on the current chart: ",iBars(Symbol(),Period()));
//---
   return(rates_total);
  }
//+------------------------------------------------------------------+
