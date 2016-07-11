//+------------------------------------------------------------------+
//|                                               MovingAverages.mqh |
//|                   Copyright 2009-2013, MetaQuotes Software Corp. |
//|                                              http://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "2009, MetaQuotes Software Corp."
#property link      "http://www.mql5.com"
//+------------------------------------------------------------------+
//| Simple Moving Average                                            |
//+------------------------------------------------------------------+
double SimpleMA(const int position,const int period,const double &price[])
  {
//---
   double result=0.0;
//--- check position
   if(position>=period-1 && period>0)
     {
      //--- calculate value
      for(int i=0;i<period;i++) result+=price[position-i];
      result/=period;
     }
//---
   return(result);
  }
//+------------------------------------------------------------------+
//| Exponential Moving Average                                       |
//+------------------------------------------------------------------+
double ExponentialMA(const int position,const int period,const double prev_value,const double &price[])
  {
//---
   double result=0.0;
//--- calculate value
   if(period>0)
     {
      double pr=2.0/(period+1.0);
      result=price[position]*pr+prev_value*(1-pr);
     }
//---
   return(result);
  }
//+------------------------------------------------------------------+
//| Smoothed Moving Average                                          |
//+------------------------------------------------------------------+
double SmoothedMA(const int position,const int period,const double prev_value,const double &price[])
  {
//---
   double result=0.0;
//--- check position
   if(period>0)
     {
      if(position==period-1)
        {
         for(int i=0;i<period;i++) result+=price[position-i];
         result/=period;
        }
      if(position>=period)
         result=(prev_value*(period-1)+price[position])/period;
     }
//---
   return(result);
  }
//+------------------------------------------------------------------+
//| Linear Weighted Moving Average                                   |
//+------------------------------------------------------------------+
double LinearWeightedMA(const int position,const int period,const double &price[])
  {
//---
   double result=0.0,sum=0.0;
   int    i,wsum=0;
//--- calculate value
   if(position>=period-1 && period>0)
     {
      for(i=period;i>0;i--)
        {
         wsum+=i;
         sum+=price[position-i+1]*(period-i+1);
        }
      result=sum/wsum;
     }
//---
   return(result);
  }
//+------------------------------------------------------------------+
//| Simple moving average on price array                             |
//+------------------------------------------------------------------+
int SimpleMAOnBuffer(const int rates_total,const int prev_calculated,const int begin,
                     const int period,const double& price[],double& buffer[])
  {
   int i,limit;
//--- check for data
   if(period<=1 || rates_total-begin<period) return(0);
//--- save as_series flags
   bool as_series_price=ArrayGetAsSeries(price);
   bool as_series_buffer=ArrayGetAsSeries(buffer);
   if(as_series_price)  ArraySetAsSeries(price,false);
   if(as_series_buffer) ArraySetAsSeries(buffer,false);
//--- first calculation or number of bars was changed
   if(prev_calculated==0) // first calculation
     {
      limit=period+begin;
      //--- set empty value for first bars
      for(i=0;i<limit-1;i++) buffer[i]=0.0;
      //--- calculate first visible value
      double firstValue=0;
      for(i=begin;i<limit;i++)
         firstValue+=price[i];
      firstValue/=period;
      buffer[limit-1]=firstValue;
     }
   else limit=prev_calculated-1;
//--- main loop
   for(i=limit;i<rates_total;i++)
      buffer[i]=buffer[i-1]+(price[i]-price[i-period])/period;
//--- restore as_series flags
   if(as_series_price)  ArraySetAsSeries(price,true);
   if(as_series_buffer) ArraySetAsSeries(buffer,true);
//---
    return(rates_total);
  }
//+------------------------------------------------------------------+
//|  Exponential moving average on price array                       |
//+------------------------------------------------------------------+
int ExponentialMAOnBuffer(const int rates_total,const int prev_calculated,const int begin,
                          const int period,const double& price[],double& buffer[])
  {
   int    i,limit;
//--- check for data
   if(period<=1 || rates_total-begin<period) return(0);
   double dSmoothFactor=2.0/(1.0+period);
//--- save as_series flags
   bool as_series_price=ArrayGetAsSeries(price);
   bool as_series_buffer=ArrayGetAsSeries(buffer);
   if(as_series_price)  ArraySetAsSeries(price,false);
   if(as_series_buffer) ArraySetAsSeries(buffer,false);
//--- first calculation or number of bars was changed
   if(prev_calculated==0)
     {
      limit=period+begin;
      //--- set empty value for first bars
      for(i=0;i<begin;i++) buffer[i]=0.0;
      //--- calculate first visible value
      buffer[begin]=price[begin];
      for(i=begin+1;i<limit;i++)
         buffer[i]=price[i]*dSmoothFactor+buffer[i-1]*(1.0-dSmoothFactor);
     }
   else limit=prev_calculated-1;
//--- main loop
   for(i=limit;i<rates_total;i++)
      buffer[i]=price[i]*dSmoothFactor+buffer[i-1]*(1.0-dSmoothFactor);
//--- restore as_series flags
   if(as_series_price)  ArraySetAsSeries(price,true);
   if(as_series_buffer) ArraySetAsSeries(buffer,true);
//---
    return(rates_total);
  }
//+------------------------------------------------------------------+
//|  Linear weighted moving average on price array                   |
//+------------------------------------------------------------------+
int LinearWeightedMAOnBuffer(const int rates_total,const int prev_calculated,const int begin,
                             const int period,const double& price[],double& buffer[],int &weightsum)
  {
   int        i,limit;
   double     sum;
//--- check for data
   if(period<=1 || rates_total-begin<period) return(0);
//--- save as_series flags
   bool as_series_price=ArrayGetAsSeries(price);
   bool as_series_buffer=ArrayGetAsSeries(buffer);
   if(as_series_price)  ArraySetAsSeries(price,false);
   if(as_series_buffer) ArraySetAsSeries(buffer,false);
//--- first calculation or number of bars was changed
   if(prev_calculated==0)
     {
      weightsum=0;
      limit=period+begin;
      //--- set empty value for first bars
      for(i=0;i<limit;i++) buffer[i]=0.0;
      //--- calculate first visible value
      double firstValue=0;
      for(i=begin;i<limit;i++)
        {
         int k=i-begin+1;
         weightsum+=k;
         firstValue+=k*price[i];
        }
      firstValue/=(double)weightsum;
      buffer[limit-1]=firstValue;
     }
   else limit=prev_calculated-1;
//--- main loop
   for(i=limit;i<rates_total;i++)
     {
      sum=0;
      for(int j=0;j<period;j++) sum+=(period-j)*price[i-j];
      buffer[i]=sum/weightsum;
     }
//--- restore as_series flags
   if(as_series_price)  ArraySetAsSeries(price,true);
   if(as_series_buffer) ArraySetAsSeries(buffer,true);
//---
    return(rates_total);
  }
//+------------------------------------------------------------------+
//|  Smoothed moving average on price array                          |
//+------------------------------------------------------------------+
int SmoothedMAOnBuffer(const int rates_total,const int prev_calculated,const int begin,
                       const int period,const double& price[],double& buffer[])
  {
   int i,limit;
//--- check for data
   if(period<=1 || rates_total-begin<period) return(0);
//--- save as_series flags
   bool as_series_price=ArrayGetAsSeries(price);
   bool as_series_buffer=ArrayGetAsSeries(buffer);
   if(as_series_price)  ArraySetAsSeries(price,false);
   if(as_series_buffer) ArraySetAsSeries(buffer,false);
//--- first calculation or number of bars was changed
   if(prev_calculated==0)
     {
      limit=period+begin;
      //--- set empty value for first bars
      for(i=0;i<limit-1;i++) buffer[i]=0.0;
      //--- calculate first visible value
      double firstValue=0;
      for(i=begin;i<limit;i++)
         firstValue+=price[i];
      firstValue/=period;
      buffer[limit-1]=firstValue;
     }
   else limit=prev_calculated-1;
//--- main loop
   for(i=limit;i<rates_total;i++)
      buffer[i]=(buffer[i-1]*(period-1)+price[i])/period;
//--- restore as_series flags
   if(as_series_price)  ArraySetAsSeries(price,true);
   if(as_series_buffer) ArraySetAsSeries(buffer,true);
//---
    return(rates_total);
  }
//+------------------------------------------------------------------+
