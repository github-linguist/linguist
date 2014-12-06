/*
License
Copyright [2013] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
*/

var TagsTotalPerMonth;

TagsTotalPerMonth = (function(){
  function TagsTotalPerMonth(){};
  
  TagsTotalPerMonth.getDatasource = function (category, months, values){
    return new CategoryMonthlyExpenseBarChartDataSource(category, months, values);
  };
  
  TagsTotalPerMonth.getType = function (){ return Charts.ChartType.COLUMN};
  
  return TagsTotalPerMonth;
})();


var TagsTotalPerMonthWithMean;

TagsTotalPerMonthWithMean = (function(){
  function TagsTotalPerMonthWithMean(){};
  
  TagsTotalPerMonthWithMean.getDatasource = function (category, months, values){
    return new CategoryMonthlyWithMeanExpenseDataSource(category, months, values);
  };
  
  TagsTotalPerMonthWithMean.getType = function (){ return Charts.ChartType.LINE};
  
  return TagsTotalPerMonthWithMean;
})();


var TagsAccumulatedPerMonth;

TagsAccumulatedPerMonth = (function(){
  function TagsAccumulatedPerMonth(){};
  
  TagsAccumulatedPerMonth.getDatasource = function (category, months, values){
    return new CategoryMonthlyAccumulated(category, months, values);
  };
  
  TagsAccumulatedPerMonth.getType = function (){ return Charts.ChartType.AREA};
  
  return TagsAccumulatedPerMonth;
})();

var MonthTotalsPerTags;

MonthTotalsPerTags = (function(){
  function MonthTotalsPerTags(){};

  MonthTotalsPerTags.getDatasource = function (month, tags, values){
    return new CategoryExpenseDataSource(tags, month, values); 
  };
  
  MonthTotalsPerTags.getType = function (){ return Charts.ChartType.PIE; };
  
  return MonthTotalsPerTags;
})();

var SavingsFlowChartComposer = (function(){
  function SavingsFlowChartComposer(){};
  
  SavingsFlowChartComposer.getDatasource = function(months, values){
    return new SavingsFlowDataSource(months, values);
  };
  
  SavingsFlowChartComposer.getType = function(){ return Charts.ChartType.COLUMN; };
  
  return SavingsFlowChartComposer;
})();
