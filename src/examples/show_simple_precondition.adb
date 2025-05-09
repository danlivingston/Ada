with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Simple_Precondition is
   pragma Assertion_Policy (Check);
   procedure DB_Entry (Name : String; Age : Natural)
   with Pre => Name'Length > 0
   is
   begin
      --  Missing implementation
      --  null;
      Put_Line ("Name: " & Name & ", Age: " & Age'Image);
   end DB_Entry;
begin
   DB_Entry ("John", 30);

   --  Precondition will fail!
   --  It doesn't??? whyy
   DB_Entry ("", 21);
end Show_Simple_Precondition;
