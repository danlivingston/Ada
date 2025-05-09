with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Simple_Contract is
   pragma Assertion_Policy (Check);
   type Int_8 is range -2**7 .. 2**7 - 1;

   function Square (A : Int_8) return Int_8
   is (A * A)
   with
     Pre  =>
       (Integer'Size >= Int_8'Size * 2
        and then Integer (A) * Integer (A) <= Integer (Int_8'Last)),
     Post =>
       (if abs A in 0 | 1 then Square'Result = abs A else Square'Result > A);

   V : Int_8;
begin
   V := Square (11);
   Put_Line ("Square of 11 is " & Int_8'Image (V));

   --  Precondition will fail...
   V := Square (12);
   Put_Line ("Square of 12 is " & Int_8'Image (V));
end Show_Simple_Contract;
