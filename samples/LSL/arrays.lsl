default {
    state_entry() {
        list lst = ["1", "2", "3"];
        llSay(0, "Create and Initialize a List\nList=["+llList2CSV(lst)+"]\n");

        lst += ["A", "B", "C"];
        llSay(0, "Append to List\nList=["+llList2CSV(lst)+"]\n");

        lst = llListInsertList(lst, ["4", "5", "6"], 3);
        llSay(0, "List Insertion\nList=["+llList2CSV(lst)+"]\n");

        lst = llListReplaceList(lst, ["a", "b", "c"], 3, 5);
        llSay(0, "Replace a portion of a list\nList=["+llList2CSV(lst)+"]\n");

        lst = llListRandomize(lst, 1);
        llSay(0, "Randomize a List\nList=["+llList2CSV(lst)+"]\n");

        lst = llListSort(lst, 1, TRUE);
        llSay(0, "Sort a List\nList=["+llList2CSV(lst)+"]\n");

        lst = [1, 2.0, "string", (key)NULL_KEY, ZERO_VECTOR, ZERO_ROTATION];
        string sCSV = llList2CSV(lst);
        llSay(0, "Serialize a List of different datatypes to a string\n(integer, float, string, key, vector, rotation)\nCSV=\""+sCSV+"\"\n");

        lst = llCSV2List(sCSV);
        llSay(0, "Deserialize a string CSV List\n(note that all elements are now string datatype)\nList=["+llList2CSV(lst)+"]\n");
    }
}
