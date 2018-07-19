   function Yes_Or_No (Prompt : String := "Your answer (Y/N): ") return Boolean is
      Answer : Character;
   begin
      Ada.Text_IO.Put (Prompt);
      loop
         Ada.Text_IO.Get_Immediate (Answer);
         case Answer is
            when 'Y'|'y' => return True;
            when 'N'|'n' => return False;
            when others  => null;
         end case;
      end loop;
   end Yes_Or_No;
