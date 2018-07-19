// 0.9-pre compatible
fn main()
{
        let q = 34u8;
        let p = 44u8;
        let l = [
        "fn main()",
        "{",
        "        let q = 34u8;",
        "        let p = 44u8;",
        "        let l = [",
        "        ",
        "        ];",
        "        let mut i = 0;",
        "        while i < 5",
        "        {",
        "                println(l[i]);",
        "                i+=1;",
        "        }",
        "        i = 0;",
        "        while i < l.len()",
        "        {",
        "                print(l[5]);",
        "                print((q as char).to_str());",
        "                print(l[i]);",
        "                print((q as char).to_str());",
        "                println((p as char).to_str());",
        "                i+=1;",
        "        }",
        "        i = 6;",
        "        while i < l.len()",
        "        {",
        "                println(l[i]);",
        "                i+=1;",
        "        }",
        "}",
        ];
        let mut i = 0;
        while i < 5
        {
                println(l[i]);
                i+=1;
        }
        i = 0;
        while i < l.len()
        {
                print(l[5]);
                print((q as char).to_str());
                print(l[i]);
                print((q as char).to_str());
                println((p as char).to_str());
                i+=1;
        }
        i = 6;
        while i < l.len()
        {
                println(l[i]);
                i+=1;
        }
}
