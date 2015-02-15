public static String select(List<String> list, String prompt){
    if(list.size() == 0) return "";
    Scanner sc = new Scanner(System.in);
    String ret = null;
    do{
        for(int i=0;i<list.size();i++){
            System.out.println(i + ": "+list.get(i));
        }
        System.out.print(prompt);
        int index = sc.nextInt();
        if(index >= 0 && index < list.size()){
            ret = list.get(index);
        }
    }while(ret == null);
    return ret;
}
