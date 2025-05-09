with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Int_Sqrt_Contract is
   pragma Assertion_Policy (Check);
   function Int_Sqrt (N : Integer) return Integer
   with
     Pre  => N >= 0,
     Post => (Int_Sqrt'Result**2 <= N) and then ((Int_Sqrt'Result + 1)**2 > N)
   is
      I : Integer := 0;
   begin
      while (I + 1) * (I + 1) <= N loop
         I := I + 1;
      end loop;
      return I;
   end Int_Sqrt;

   Input  : Integer;
   Output : Integer;

begin
   Put ("Enter a non-negative integer: ");
   Get (Input);

   Output := Int_Sqrt (Input);

   Put ("Integer square root is: ");
   Put (Output);
   New_Line;
end Int_Sqrt_Contract;
