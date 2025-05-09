with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Fibonacci is
   function Fibonacci_Number (N : Integer) return Integer is
   begin
      if N <= 0 then
         return 0;
      elsif N = 1 then
         return 1;
      else
         return Fibonacci_Number (N - 1) + Fibonacci_Number (N - 2);
      end if;
   end Fibonacci_Number;

   Input  : Integer;
   Result : Integer;
begin
   Put_Line ("Enter a number to calculate its Fibonacci value:");
   Get (Input);

   if Input < 0 then
      Put_Line ("Please enter a non-negative integer.");
   else
      Result := Fibonacci_Number (Input);
      Put_Line ("The Fibonacci number is: " & Integer'Image (Result));
   end if;
end Fibonacci;
