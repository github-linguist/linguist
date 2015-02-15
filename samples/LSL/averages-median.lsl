integer MAX_ELEMENTS = 10;
integer MAX_VALUE = 100;
default {
    state_entry() {
        list lst = [];
        integer x = 0;
        for(x=0 ; x<MAX_ELEMENTS ; x++) {
            lst += llFrand(MAX_VALUE);
        }
        llOwnerSay("lst=["+llList2CSV(lst)+"]");
        llOwnerSay("Geometric Mean: "+(string)llListStatistics(LIST_STAT_GEOMETRIC_MEAN, lst));
        llOwnerSay("           Max: "+(string)llListStatistics(LIST_STAT_MAX, lst));
        llOwnerSay("          Mean: "+(string)llListStatistics(LIST_STAT_MEAN, lst));
        llOwnerSay("        Median: "+(string)llListStatistics(LIST_STAT_MEDIAN, lst));
        llOwnerSay("           Min: "+(string)llListStatistics(LIST_STAT_MIN, lst));
        llOwnerSay("     Num Count: "+(string)llListStatistics(LIST_STAT_NUM_COUNT, lst));
        llOwnerSay("         Range: "+(string)llListStatistics(LIST_STAT_RANGE, lst));
        llOwnerSay("       Std Dev: "+(string)llListStatistics(LIST_STAT_STD_DEV, lst));
        llOwnerSay("           Sum: "+(string)llListStatistics(LIST_STAT_SUM, lst));
        llOwnerSay("   Sum Squares: "+(string)llListStatistics(LIST_STAT_SUM_SQUARES, lst));
    }
}
