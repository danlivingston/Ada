with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Simple_Postcondition is
   pragma Assertion_Policy (Check);
   type Int_8 is range -2**7 .. 2**7 - 1;

   type Int_8_Array is array (Integer range <>) of Int_8;

   function Square (A : Int_8) return Int_8
   is (A * A)
   with
     Post =>
       (if abs A in 0 | 1 then Square'Result = abs A else Square'Result > A);

   procedure Square (A : in out Int_8_Array)
   with Post => (for all I in A'Range => A (I) = A'Old (I) * A'Old (I))
   is
   begin
      for V of A loop
         V := Square (V);
      end loop;
   end Square;

   V : Int_8_Array := (-2, -1, 0, 1, 10, 11);
begin
   for E of V loop
      Put_Line ("Original: " & Int_8'Image (E));
   end loop;
   New_Line;

   Square (V);
   for E of V loop
      Put_Line ("Square:   " & Int_8'Image (E));
   end loop;
end Show_Simple_Postcondition;
